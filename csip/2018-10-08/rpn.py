def rpn(expr):
    tokens = expr.split()
    stack = []

    for tok in tokens:
        if tok == '+-*/':
            rhs = stack.pop()
            lhs = stack.pop()
            val = eval(f'{lhs} {tok} {rhs}')
            stack.append(val)
        else:
            val = float(tok)
            stack.append(val)

    assert len(stack) == 1
    return stack[0]
