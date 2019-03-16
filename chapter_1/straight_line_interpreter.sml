type id = string;

datatype binary_operator = plus | minus | times | divide;

datatype statement = compound_statement of statement * statement
                   | assignment_statement of id * expression
                   | print_statement of expression list
    and
        expression = id_expression of id
                   | number_expression of int
                   | operation_expression of expression * binary_operator * expression
                   | sequence_expression of statement * expression;


(* a := 5+3; b:= (print(a, a-1), 10*a); print(b) *)
val my_program = compound_statement(
                   assignment_statement("a",
                     operation_expression(number_expression(5),
                                          plus,
                                          number_expression(3))),
                   compound_statement(assignment_statement("b",
                       sequence_expression(print_statement([id_expression("a"),
                       operation_expression(id_expression("a"), minus,
                       number_expression(1))]),
                       operation_expression(number_expression(10), times,
                       id_expression("a")))),
                       print_statement([id_expression("b")])));

type table = (id * int) list;

exception UnboundVariableError

fun lookup(nil, my_id) = raise UnboundVariableError
|   lookup((this_id, this_value)::xs : table, my_id) =
        if this_id = my_id then this_value
        else lookup(xs, my_id);

fun update(my_table:table, my_id, my_value) = (my_id, my_value)::my_table;

fun statement_interprete(my_statement:statement, e:table) =
  case my_statement of compound_statement(statement_1:statement, statement_2:statement) =>
                           let
                               val e_prime = statement_interprete(statement_1, e)
                           in
                               statement_interprete(statement_2, e_prime)
                           end
                       | assignment_statement(my_id, my_expression) =>
                           let
                               val (my_result, e_prime) = expression_interprete(my_expression, e)
                           in
                               update(e_prime, my_id, my_result)
                           end
                       | print_statement(nil) => e
                       | print_statement(my_expression::my_expression_list : expression list) =>
                             let
                                 val (my_value, e_prime) = expression_interprete(my_expression, e)
                             in
                                 (print(Int.toString(my_value)^"\n");
                                  statement_interprete(print_statement(my_expression_list), e_prime)
                                  )
                             end

                       (*    (print(Int.toString(#1(expression_interprete(hd(my_expression),
                       *    e)));
                       *    statement_interprete(print_statement(tl(my_expression)),
                       *    e); e) *)
and expression_interprete(my_expression:expression, e:table) =
    case my_expression of id_expression(my_id) => (lookup(e, my_id), e)
                       | number_expression(my_number) => (my_number, e)
                       | operation_expression(exp_1, plus, exp_2)   =>
                           (#1(expression_interprete(exp_1, e)) + #1(expression_interprete(exp_2, e)), e)
                       | operation_expression(exp_1, minus, exp_2)  =>
                           (#1(expression_interprete(exp_1, e)) - #1(expression_interprete(exp_2, e)), e)
                       | operation_expression(exp_1, times, exp_2)  =>
                           (#1(expression_interprete(exp_1, e)) * #1(expression_interprete(exp_2, e)), e)
                       | operation_expression(exp_1, divide, exp_2) =>
                           (#1(expression_interprete(exp_1, e)) div #1(expression_interprete(exp_2, e)), e)
                       | sequence_expression(statement_1, exp)      =>
                             let
                                 val e_prime = statement_interprete(statement_1, e)
                             in
                                 expression_interprete(exp, e_prime)
                             end
                       ;

fun straight_line_interprete(program : statement) = statement_interprete(program, nil);

straight_line_interprete(my_program);
