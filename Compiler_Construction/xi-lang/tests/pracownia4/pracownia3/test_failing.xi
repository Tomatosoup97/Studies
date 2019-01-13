pred(result:int):bool {
	return result == 13
}

main():int {
	result:int = 13
	while pred(result) {
		result = 100
	}
	return result
}

//@PRACOWNIA
//@out Exit code: 100
