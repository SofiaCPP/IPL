module.exports = grammar({
    name: 'Calc',
    rules: {
        source_file: $ => $.expression,

        expression: $ => choice(
            $.term,
            $.unary_expression,
            $.binary_expression,
        ),

        unary_expression: $ => prec.left(1, seq('-', $.term)),

        binary_expression: $ => choice(
            prec.left(2, seq($.expression, '+', $.expression)),
            prec.left(2, seq($.expression, '-', $.expression)),
            prec.left(3, seq($.expression, '*', $.expression)),
            prec.left(3, seq($.expression, '/', $.expression)),
        ),

        term: $ => choice(
            $.identifier,
            $.number,
            seq('(', $.expression, ')'),
        ),

        identifier: $ => /[a-z_]+/,
        number: $ => /[0-9][0-9.]*/,
    }
});
