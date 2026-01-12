// --- TruMyth Language Interpreter v6.1 ---
use std::collections::HashMap;
use std::io::{self, Write};

// --- 1. DATA MODELS ---
#[derive(Debug, Clone, PartialEq)]
enum Type { Int, Text, TypeError }

#[derive(Debug, Clone)]
enum Value { Int(i32), Text(String) }

#[derive(Debug, Clone)]
enum Expr {
    Number(i32),
    Variable(String),
    LiteralStr(String),
    BinOp(Box<Expr>, String, Box<Expr>),
    Call(String, Vec<Expr>), // The Teleporter
}

#[derive(Debug, Clone)]
struct FunctionData {
    params: Vec<String>,
    body_expr: Expr,
}

// --- 2. THE TYPE CHECKER ---
fn check_types(expr: &Expr, vars: &HashMap<String, Type>, funcs: &HashMap<String, FunctionData>) -> Type {
    match expr {
        Expr::Number(_) => Type::Int,
        Expr::LiteralStr(_) => Type::Text,
        Expr::Variable(v) => vars.get(v).cloned().unwrap_or(Type::Int),
        Expr::Call(name, _) => {
            if funcs.contains_key(name) { Type::Int } else { Type::TypeError }
        }
        Expr::BinOp(l, op, r) => {
            let left_type = check_types(l, vars, funcs);
            let right_type = check_types(r, vars, funcs);
            match op.as_str() {
                "+" => if left_type == Type::Int && right_type == Type::Int { Type::Int } 
                       else if left_type == Type::Text && right_type == Type::Text { Type::Text } 
                       else { Type::TypeError },
                _ => if left_type == Type::Int && right_type == Type::Int { Type::Int } else { Type::TypeError },
            }
        }
    }
}

// --- 3. THE PARSER (heh purse) ---
fn parse_expr(tokens: &mut Vec<String>) -> Expr {
    let mut left = parse_math(tokens);
    while !tokens.is_empty() && (tokens[0] == "==" || tokens[0] == ">" || tokens[0] == "<") {
        let op = tokens.remove(0);
        let right = parse_math(tokens);
        left = Expr::BinOp(Box::new(left), op, Box::new(right));
    }
    left
}

fn parse_math(tokens: &mut Vec<String>) -> Expr {
    let mut left = parse_term(tokens);
    while !tokens.is_empty() && (tokens[0] == "+" || tokens[0] == "-") {
        let op = tokens.remove(0);
        let right = parse_term(tokens);
        left = Expr::BinOp(Box::new(left), op, Box::new(right));
    }
    left
}

fn parse_term(tokens: &mut Vec<String>) -> Expr {
    let mut left = parse_primary(tokens);
    while !tokens.is_empty() && (tokens[0] == "*" || tokens[0] == "/") {
        let op = tokens.remove(0);
        let right = parse_primary(tokens);
        left = Expr::BinOp(Box::new(left), op, Box::new(right));
    }
    left
}

fn parse_primary(tokens: &mut Vec<String>) -> Expr {
    if tokens.is_empty() { return Expr::Number(0); }
    let t = tokens.remove(0);
    
    if t.starts_with('"') { 
        Expr::LiteralStr(t[1..t.len()-1].to_string()) 
    } else if t == "(" { 
        let e = parse_expr(tokens); 
        if !tokens.is_empty() { tokens.remove(0); } 
        e 
    } else if let Ok(n) = t.parse::<i32>() { 
        Expr::Number(n) 
    } else {
        // FUNCTION CALL DETECTOR
        if !tokens.is_empty() && tokens[0] == "(" {
            tokens.remove(0); 
            let mut args = Vec::new();
            while !tokens.is_empty() && tokens[0] != ")" {
                args.push(parse_expr(tokens));
                if !tokens.is_empty() && tokens[0] == "," { tokens.remove(0); }
            }
            if !tokens.is_empty() { tokens.remove(0); }
            Expr::Call(t, args)
        } else {
            Expr::Variable(t)
        }
    }
}

// --- 4. THE EVALUATOR ---
fn evaluate(expr: Expr, vars: &HashMap<String, Value>, funcs: &HashMap<String, FunctionData>) -> Value {
    match expr {
        Expr::Number(n) => Value::Int(n),
        Expr::LiteralStr(s) => Value::Text(s),
        Expr::Variable(v) => vars.get(&v).cloned().unwrap_or(Value::Int(0)),
        Expr::BinOp(l, op, r) => {
            let lv = evaluate(*l, vars, funcs);
            let rv = evaluate(*r, vars, funcs);
            match op.as_str() {
                "+" => match (lv, rv) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                    (Value::Text(a), Value::Text(b)) => Value::Text(format!("{}{}", a, b)),
                    _ => Value::Int(0),
                },
                "-" => if let (Value::Int(a), Value::Int(b)) = (lv, rv) { Value::Int(a - b) } else { Value::Int(0) },
                "*" => if let (Value::Int(a), Value::Int(b)) = (lv, rv) { Value::Int(a * b) } else { Value::Int(0) },
                "/" => if let (Value::Int(a), Value::Int(b)) = (lv, rv) { Value::Int(if b != 0 { a / b } else { 0 }) } else { Value::Int(0) },
                "==" => Value::Int(if format!("{:?}", lv) == format!("{:?}", rv) {1} else {0}),
                ">" => if let (Value::Int(a), Value::Int(b)) = (lv, rv) { Value::Int(if a > b {1} else {0}) } else { Value::Int(0) },
                "<" => if let (Value::Int(a), Value::Int(b)) = (lv, rv) { Value::Int(if a < b {1} else {0}) } else { Value::Int(0) },
                _ => Value::Int(0),
            }
        }
        Expr::Call(name, args) => {
            if let Some(fd) = funcs.get(&name) {
                let mut local_vars = HashMap::new();
                for (param_name, arg_expr) in fd.params.iter().zip(args) {
                    let val = evaluate(arg_expr, vars, funcs);
                    local_vars.insert(param_name.clone(), val);
                }
                evaluate(fd.body_expr.clone(), &local_vars, funcs)
            } else {
                Value::Int(0)
            }
        }
    }
}

// --- 5. THE TOKENIZER (chocalate!?)---
fn tokenize(s: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = s.chars().peekable();
    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' | '\n' | ',' => { chars.next(); }
            '"' => {
                chars.next();
                let mut s = String::new();
                while let Some(&inner) = chars.peek() {
                    if inner == '"' { chars.next(); break; }
                    s.push(chars.next().unwrap());
                }
                tokens.push(format!("\"{}\"", s));
            }
            '=' | '>' | '<' => {
                let first = chars.next().unwrap();
                if chars.peek() == Some(&'=') { chars.next(); tokens.push(format!("{}=", first)); }
                else { tokens.push(first.to_string()); }
            }
            '+' | '-' | '*' | '/' | '(' | ')' | ':' => { tokens.push(chars.next().unwrap().to_string()); }
            _ if c.is_alphanumeric() => {
                let mut t = String::new();
                while let Some(&ic) = chars.peek() {
                    if ic.is_alphanumeric() { t.push(chars.next().unwrap()); }
                    else { break; }
                }
                tokens.push(t);
            }
            _ => { chars.next(); }
        }
    }
    tokens
}

// --- 6. THE COMMAND EXECUTER ---
fn run_command(line: &str, vars: &mut HashMap<String, Value>, types: &mut HashMap<String, Type>, funcs: &HashMap<String, FunctionData>) {
    let line = line.trim();
    if line.is_empty() { return; }
    
    if line.starts_with("print(") && line.ends_with(')') {
        let inner = &line[6..line.len()-1];
        let mut tokens = tokenize(inner);
        let ast = parse_expr(&mut tokens);
        if check_types(&ast, types, funcs) != Type::TypeError {
            match evaluate(ast, vars, funcs) {
                Value::Int(n) => println!("{}", n),
                Value::Text(s) => println!("{}", s),
            }
        } else { println!("\x1b[0;31m[TYPE ERROR]\x1b[0m"); }
    } else if line.contains('=') {
        let tokens = tokenize(line);
        if let Some(pos) = tokens.iter().position(|t| t == "=") {
            let var_name = tokens[..pos].join("").trim().to_string();
            let mut expr_tokens = tokens[pos+1..].to_vec();
            let ast = parse_expr(&mut expr_tokens);
            let t = check_types(&ast, types, funcs);
            if t != Type::TypeError {
                let val = evaluate(ast, vars, funcs);
                types.insert(var_name.clone(), t);
                vars.insert(var_name, val);
            } else { println!("\x1b[0;31m[TYPE ERROR]\x1b[0m"); }
        }
    }
}

// --- 7. MAIN REPL ---
fn main() {
    let mut variables = HashMap::new();
    let mut type_map = HashMap::new();
    let mut functions = HashMap::new();
    println!("TRUMYTH v6.1 - THE GOAT EDITION");

    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let line = input.trim();
        if line == "exit" { break; }

        if line.starts_with("func ") {
            let parts: Vec<&str> = line[5..].split(':').collect();
            if parts.len() == 2 {
                let header = parts[0].trim();
                let body_str = parts[1].trim();
                let name_end = header.find('(').unwrap_or(header.len());
                let func_name = header[..name_end].trim().to_string();
                let params = if let Some(start) = header.find('(') {
                    let end = header.find(')').unwrap_or(header.len());
                    header[start+1..end].split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()).collect()
                } else { Vec::new() };
                let mut body_tokens = tokenize(body_str);
                let body_ast = parse_expr(&mut body_tokens);
                functions.insert(func_name.clone(), FunctionData { params, body_expr: body_ast });
                println!("\x1b[0;32m[SYSTEM] Function '{}' online.\x1b[0m", func_name);
            }
        } else if line.starts_with("if ") {
            let (if_part, else_part) = if line.contains(" else ") {
                let s: Vec<&str> = line.splitn(2, " else ").collect();
                (s[0], Some(s[1]))
            } else { (line, None) };
            let parts: Vec<&str> = if_part[3..].split(':').collect();
            if parts.len() == 2 {
                let mut cond_tokens = tokenize(parts[0]);
                let cond_ast = parse_expr(&mut cond_tokens);
                if let Value::Int(1) = evaluate(cond_ast, &variables, &functions) {
                    run_command(parts[1], &mut variables, &mut type_map, &functions);
                } else if let Some(e_action) = else_part {
                    run_command(e_action, &mut variables, &mut type_map, &functions);
                }
            }
        } else if line.starts_with("while ") {
            let parts: Vec<&str> = line[6..].split(':').collect();
            if parts.len() == 2 {
                loop {
                    let mut cond_tokens = tokenize(parts[0]);
                    let cond_ast = parse_expr(&mut cond_tokens);
                    if let Value::Int(1) = evaluate(cond_ast, &variables, &functions) {
                        run_command(parts[1], &mut variables, &mut type_map, &functions);
                    } else { break; }
                }
            }
        } else {
            run_command(line, &mut variables, &mut type_map, &functions);
        }
    }
}