def and(A: Int, B: Int) : Int =
{
    if (A == -1 || B == -1) then -1 else (A && B)
}

def or(A: Int, B: Int) : Int =
{
    if (A + B <= -1) then -1 else (A || B)
}

def xor(A: Int, B: Int) : Int =
{
    if (A == -1 || B == -1) then -1 else (A ^ B)
}

def not(A: Int, B: Int) : Int =
{
    if (A == -1) then -1 else !A
}


