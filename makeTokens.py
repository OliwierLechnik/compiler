if __name__ == "__main__":
    with open("tokens.txt") as f:
        lines = f.readlines()
        for line in lines:
            word = line[0:-1]
            print(f"lex{word.title()} :: Parser Text")
            print(f"lex{word.title()} = keyword \"{word}\"")
            print()
