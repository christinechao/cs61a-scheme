def list_partitions(total, max_pieces, max_value):
    """
    >>> list_partitions(1, 1, 1)
    [[1]]
    >>> list_partitions(1, 2, 1)
    [[1]]
    >>> list_partitions(10, 0, 1)
    []
    >>> list_partitions(10, 1, 1)
    []
    >>> list_partitions(10, 1, 10)
    [[10]]
    >>> list_partitions(2, 2, 1)
    [[1, 1]]
    >>> list_partitions(2, 2, 2)
    [[1, 1], [2]]
    >>> list_partitions(3, 3, 3)
    [[1, 1, 1], [2, 1], [3]]
    """
    def helper(t, mp, mv, sofar, partitions):
        # print(t, mp, mv, sofar, partitions)
        if t == 0 and sofar != []:
            partitions = partitions + [sofar]
        if mp <= 0 or mv <= 0 or t <= 0:
            return partitions
        old_parts = partitions
        for i in range(1, mv + 1):
            new_parts = helper(t-i, mp-1, i, sofar + [i], old_parts)
            new_parts = [e for e in new_parts if e != []]
            partitions = partitions + new_parts
        # print(t, mp, mv, sofar, partitions)
        return partitions

    return helper(total, max_pieces, max_value, [], [])

