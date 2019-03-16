type id = string;

datatype binary_operator = plus | minus | times | division;

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


