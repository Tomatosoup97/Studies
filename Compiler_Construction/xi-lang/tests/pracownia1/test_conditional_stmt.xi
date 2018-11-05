f()
{
    if (x > 10) y = 1
    if x > 10 y = 1
    if x > 10 {
        return "42"
    }
    if pred() y = 1
    if pred() & y | z x = 1
}

f()
{
    if (x > 10) y = 1 else z = 1
    if x > 10 y = 1 else z = 1
    if x > 10 { 
        return "42"
    } else z = 1

    if pred() y = 1 else {
        z = 1
    }
    if pred() & y | z x = 1 else {
        z = 3
    }
}

f()
{
    if x if y z = 1 else z = 2
}

f()
{
    if x if y z = 1 else z = 2 else z = 1
}

f()
{
    if a if x if y z = 1 else z = 2 else z = 1
}


//@PRACOWNIA
//@stop_after parser
