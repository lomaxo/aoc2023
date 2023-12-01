test_data = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""
with open("input_files/aoc23-1.txt") as file:
    data = file.read()

test_data2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

def part1(data):
    results = []
    for word in data.split():
        digits = ""
        for letter in word:
            if letter.isdigit():
                digits += letter
        results.append(int(digits[0] + digits[-1]))

    return sum(results)


def word_to_digits(word: str) -> str:
    i = 0
    numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    digits = ""
    while i < len(word):
        if word[i].isdigit():
            digits += word[i]
        else:
            for n, number in enumerate(numbers):
                if word[i:].startswith(number):
                    digits += str(n+1)
        i += 1
    return digits

def part2(data):
    results = []
    for word in data.split():
        # print(word_to_digits(word))
        digits = word_to_digits(word)
        results.append(int(digits[0] + digits[-1]))
        # else:
            # results.append(int(digits))
    # print(results)
    # for result in results:
    #     if result > 99 or result < 0:
    #         print(f"warning {result}")
    return sum(results)




print(part2(data))
# print(replace_word_numbers("llnshf1"))
# print(word_to_digits("eighthree"))