def histline(n, ints):
    for i in xrange(10):
        if ints[i] >= n:
            yield '*'
        else:
            yield ' '
