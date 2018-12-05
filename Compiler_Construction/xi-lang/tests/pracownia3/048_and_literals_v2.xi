convert(b:bool):int
{
    if b {
        return 7
    } else {
        return 12
    }
}

main():int
{
    return convert(true & false)
}

//@PRACOWNIA
//@out Exit code: 12
