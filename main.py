import Tokens
while True:
        text = input('Tokens > ')
        result, error = Tokens.run('<stdin>', text)

        if error: print(error.as_string())
        else: print(result)