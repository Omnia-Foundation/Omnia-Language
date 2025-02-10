use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::llvm_sys::LLVMValue;
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::module::Module;
use inkwell::{IntPredicate, OptimizationLevel};
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::AnyType;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue};
use crate::core::errors::Failure;
use crate::parser::ast::{AssignmentStatementNode, BPrintStatementNode, BinaryExpressionNode, BinaryOperator, BlockStatementNode, ConditionalExpressionNode, ConditionalOperator, Expression, IfStatementNode, LiteralExpression, Node, Statement, VariableAccessExpressionNode, VariableDeclarationStatementNode};

type IRResult<'ir> = Result<AnyValueEnum<'ir>, Failure>;

pub struct Generator {}
impl Generator {
    pub fn generate(input: Statement, mut output: PathBuf, verbose: bool, is_obj_only: bool) {
        let context = Context::create();
        let llvm_context = LLVMContext::new(&context);
        let i32_t = llvm_context.context.i32_type();
        let i8ptr_type = llvm_context.context.ptr_type(inkwell::AddressSpace::default());
        let func_t = i32_t.fn_type(&[], false);
        let printf_type = llvm_context.context.i32_type().fn_type(&[i8ptr_type.into()], true);
        let printf_func = llvm_context.module.add_function("printf", printf_type, None);
        let func = llvm_context.module.add_function("main", func_t, None);
        let entry = llvm_context.context.append_basic_block(func, "entry");
        llvm_context.builder.position_at_end(entry);
        let mut st = llvm_context.symbol_table.take();
        st.insert("printf".to_string(), printf_func.as_any_value_enum());
        println!("table: {:?}", st);
        llvm_context.symbol_table.replace(st);
        match input {
            Statement::Block(block) => {
                for statement in block.get_stmts_ref() {
                    match statement {
                        Statement::Block(block) => { block.codegen(&llvm_context).expect(""); }
                        Statement::BPrintStatement(print) => {
                            // let print_num_type = llvm_context.context
                            //     .void_type()
                            //     .fn_type(&[llvm_context.context.i32_type().into()], false);
                            // let print_num_func = llvm_context.module.add_function("print_num", print_num_type, None);

                            print.codegen(&llvm_context).expect("unable to generate print");
                        }
                        Statement::VariableDeclaration(v) => { v.codegen(&llvm_context).expect(""); }
                        Statement::Assignment(a) => { a.codegen(&llvm_context).expect(""); }
                        Statement::If(i) => { i.codegen(&llvm_context).expect(""); }
                    }
                }
            }
            _ => ()
        }
        llvm_context.builder.build_return(None).expect("");
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)
            .map_err(|e| format!("Error while loading target backend {:?}", e)).expect("");
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "x86-64",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default
            )
            .ok_or("Unable to create TargetMachine").expect("");
        if let None = output.extension() {
            let mut binding = output.to_str().unwrap().to_string();
            binding.push_str(".o");
            output = PathBuf::from(binding);
        }
        target_machine.write_to_file(&llvm_context.module, FileType::Object, output.as_path())
            .map_err(|e| format!("Error while writing file: {:?}", e)).expect("");
        if verbose {
            llvm_context.module.print_to_stderr();
            println!("Created object file at path {:?}", output)
        }
        // if !is_obj_only {
        //     Command::new("clang")
        //         .arg(output.clone())
        //         .args(["-o", output.to_str().unwrap().replace(".o", ".exe").as_str()])
        //         .output().expect("Unable to create file");
        //     fs::remove_file(output.clone()).expect("Unable to delete file");
        //     if verbose {
        //         println!("Created executable at path {:?}", output)
        //     }
        // }


    }
}

pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    symbol_table: RefCell<HashMap<String, AnyValueEnum<'ctx>>>
}
impl <'ctx> LLVMContext<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("omnia_module");
        Self {
            context,
            builder,
            module,
            symbol_table: RefCell::new(HashMap::new())
        }
    }
}


pub trait CodeGen {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir>;
}

impl CodeGen for LiteralExpression {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        match self {
            LiteralExpression::Integer(i) => {
                let int_type = context.context.i32_type();
                Ok(int_type.const_int(*i as u64, false).as_any_value_enum())
            }
        }
    }
}
impl CodeGen for Expression {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        match self {
            Expression::Binary(b) => { b.codegen(context) }
            Expression::Literal(l) => { l.codegen(context) }
            Expression::VariableAccess(v) => v.codegen(context),
            Expression::Conditional(c) => c.codegen(context)
        }
    }
}
impl CodeGen for BinaryExpressionNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let left = self
            .left
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        let right = self
            .right
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        // println!("Operator is {}", self.oper);
        // println!("Left: {}, right: {}", left, right);
        let int_res = match self.oper {
            BinaryOperator::Plus => context.builder.build_int_add(
                left, right, &"addtmp"
            ),
            BinaryOperator::Minus => context.builder.build_int_sub(
                left, right, &"subtmp"
            ),
            BinaryOperator::Mul => context.builder.build_int_mul(
                left, right, &"multmp"
            ),
            BinaryOperator::Div => context.builder.build_int_unsigned_div(
                left, right, &"divtmp"
            ),
        };
        // println!("int_res is {:?}", int_res);
        Ok(int_res.expect("Unrecoverable: LLVM failed to generate binary expression").as_any_value_enum())

    }
}
impl CodeGen for VariableAccessExpressionNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let st = context.symbol_table.take();
        let ptr = st.get(self.get_name()).expect(format!("Cannot find variable with name `{}`", self.get_name()).leak()).into_pointer_value();
        context.symbol_table.replace(st);
        Ok(context.builder.build_load(context.context.i32_type(), ptr, self.get_name_clone().leak())?.as_any_value_enum())

    }
}

impl CodeGen for Node {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        match self {
            Node::Expression(e) => e.codegen(context),
            Node::Statement(s) => s.codegen(context)
        }
    }
}

impl CodeGen for Statement {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        match self {
            Statement::Block(b) => b.codegen(context),
            Statement::BPrintStatement(p) => {
                p.codegen(context)
            }
            Statement::VariableDeclaration(v) => v.codegen(context),
            Statement::Assignment(a) => a.codegen(context),
            Statement::If(i) => i.codegen(context)
        }
    }
}

impl CodeGen for VariableDeclarationStatementNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let val = self.get_value_ref()
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        let mut st = context.symbol_table.take();
        if st.contains_key(self.get_name()) {
            return Err(Failure::CodegenVariableAlreadyExists { name: self.get_name_clone() })
        }
        let ptr = context.builder.build_alloca(context.context.i32_type(), self.get_name_clone().leak())?;
        st.insert(self.get_name_clone(), ptr.as_any_value_enum());
        context.symbol_table.replace(st);
        Ok(context.builder.build_store(ptr, val)?.as_any_value_enum())

    }
}
impl CodeGen for AssignmentStatementNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let val = self.get_value_ref()
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        let mut st = context.symbol_table.take();
        let ptr = st.get(self.get_name()).expect(format!("Variable `{}` not initialized", self.get_name()).leak()).into_pointer_value();
        st.insert(self.get_name_clone(), ptr.as_any_value_enum());
        context.symbol_table.replace(st);
        Ok(context.builder.build_store(ptr, val)?.as_any_value_enum())
    }
}
impl CodeGen for BlockStatementNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let mut last: Option<AnyValueEnum<'ir>> = None;

        for stmt in self.get_stmts_ref() {
            let value = stmt.codegen(context)?;
            last = Some(value);
            if let AnyValueEnum::InstructionValue(inst) = last.as_ref().unwrap() {
                if inst.is_terminator() {
                    break
                }
            }
        }

        Ok(last.unwrap_or_else(|| context.context.i32_type().const_int(993, false).as_any_value_enum()))
    }
}
impl CodeGen for BPrintStatementNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let val = self.get_value_ref()
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        let st = context.symbol_table.take();
        let printf_func = st.get("printf").expect("Cannot find `printf` function").into_function_value();
        context.symbol_table.replace(st);
        let format_str = context.builder.build_global_string_ptr("%d\n", "format_str");
        let mut args = vec![format_str?.as_pointer_value().into()];
        args.extend([val.as_basic_value_enum()].iter().map(|&arg| <BasicValueEnum<'_> as Into<BasicMetadataValueEnum>>::into(arg) ));
        Ok(context.builder.build_call(printf_func, &args, "printf_call")?.as_any_value_enum())
    }
}
impl CodeGen for IfStatementNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let cond = self.get_cond_ref()
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        let block = context.builder.get_insert_block().ok_or_else(|| {
            Failure::CodegenCurrentBasicBlockIsMissing
        })?;
        let func = block.get_parent().ok_or_else(|| {
            Failure::CodegenBasicBlockNotLinkedToFunction
        })?;
        let then = context.context.append_basic_block(func, "if_then");
        let merge = context.context.append_basic_block(func, "if_merge");
        if let Some(e) = self.get_else_ref() {
            let r#else = context.context.append_basic_block(func, "if_else");
            context.builder.build_conditional_branch(cond, then, r#else)?;
            context.builder.position_at_end(then);
            self.get_then_ref().codegen(context)?;
            context.builder.build_unconditional_branch(merge)?;

            context.builder.position_at_end(r#else);
            e.codegen(context)?;
            context.builder.build_unconditional_branch(merge)?;
        } else {
            context.builder.build_conditional_branch(cond, then, merge)?;
            context.builder.position_at_end(then);
            self.get_then_ref().codegen(context)?;
            context.builder.build_unconditional_branch(merge)?;
        }
        context.builder.position_at_end(merge);
        Ok(context.context.i32_type().const_int(993, false).as_any_value_enum())
        // let then = self.get_then_ref()
        //     .codegen(context)
        //     .map(AnyValueEnum::into_function_value)?;
        // let merge_block = context.context.append_basic_block(context.module.add_function("merge", context.context.i32_type().fn_type(&[], false), None), "merge_block");
        // match self.get_else_ref() {
        //     Some(e) => {
        //         let true_block = context.context.append_basic_block(then, "true_block");
        //         let r#else = e
        //             .codegen(context)
        //             .map(AnyValueEnum::into_function_value)?;
        //         let false_block = context.context.append_basic_block(r#else, "false_block");
        //         context.builder.build_conditional_branch(cond, true_block, false_block)?;
        //         context.builder.position_at_end(true_block);
        //         context.builder.build_unconditional_branch(merge_block)?;
        //         context.builder.position_at_end(false_block);
        //         context.builder.build_unconditional_branch(merge_block)?;
        //         context.builder.position_at_end(merge_block);
        //     }
        //     None => {
        //         let true_block = context.context.append_basic_block(then, "true_block");
        //         let false_block = context.context.append_basic_block(context.module.add_function("false", context.context.i32_type().fn_type(&[], false), None), "false_block");
        //         context.builder.build_conditional_branch(cond, true_block, false_block)?;
        //         context.builder.position_at_end(true_block);
        //         context.builder.build_unconditional_branch(merge_block)?;
        //         context.builder.position_at_end(false_block);
        //         context.builder.build_unconditional_branch(merge_block)?;
        //     }
        // };
        // Ok(then.as_any_value_enum())

    }
}
impl CodeGen for ConditionalExpressionNode {
    fn codegen<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> IRResult<'ir> {
        let left = self.left
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        let right = self.right
            .codegen(context)
            .map(AnyValueEnum::into_int_value)?;
        let res = match self.oper {
            ConditionalOperator::Equals => {
                context.builder.build_int_compare(
                    IntPredicate::EQ,
                    left,
                    right,
                    "int_eq"
                )
            }
            ConditionalOperator::NotEquals => {
                context.builder.build_int_compare(
                    IntPredicate::NE,
                    left,
                    right,
                    "int_neq"
                )
            }
            ConditionalOperator::Greater => {
                context.builder.build_int_compare(
                    IntPredicate::UGT,
                    left,
                    right,
                    "int_gt"
                )
            }
            ConditionalOperator::Lesser => {
                context.builder.build_int_compare(
                    IntPredicate::ULT,
                    left,
                    right,
                    "int_lt"
                )
            }
            ConditionalOperator::GreaterEquals => {
                context.builder.build_int_compare(
                    IntPredicate::UGE,
                    left,
                    right,
                    "int_ge"
                )
            }
            ConditionalOperator::LesserEquals => {
                context.builder.build_int_compare(
                    IntPredicate::ULE,
                    left,
                    right,
                    "int_le"
                )
            }
        };
        Ok(res.expect("Unrecoverable: LLVM failed to generate conditional expression").as_any_value_enum())
    }
}
impl From<BuilderError> for Failure {
    fn from(value: BuilderError) -> Self {
        todo!()
    }
}