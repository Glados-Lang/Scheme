import subprocess
import tempfile

def colored(text, color):
    colors = {
        "red": "\033[91m",
        "green": "\033[92m",
        "yellow": "\033[93m",
        "blue": "\033[94m",
        "reset": "\033[0m",
    }
    return f"{colors.get(color, '')}{text}{colors['reset']}"


# Define categories and tests
test_categories = {
    "Evaluator": {
        "Basic Evaluation": [
            {
                "name": "evaluates numbers",
                "content": "42",
                "expected": "42\n"
            },
            {
                "name": "evaluates booleans",
                "content": "#t",
                "expected": "#t\n"
            },
            {
                "name": "evaluates simple arithmetic operations",
                "content": "(+ 1 2)",
                "expected": "3\n"
            },
            {
                "name": "evaluates nested arithmetic operations",
                "content": "(+ (* 2 3) (div 10 2))",
                "expected": "11\n"
            },
        ],
        "Error Handling": [
            {
                "name": "handles undefined symbols",
                "content": "foo",
                "expected": "Error: Undefined symbol: foo\n",
                "exit_code": 84
            },
            {
                "name": "errors on calling non-functions",
                "content": "(42 1 2)",
                "expected": "Error: Attempted to call a non-function\n",
                "exit_code": 84
            },
        ],
    },
    "Parser": {
        "Basic Parsers": [
            {
                "name": "parses numbers",
                "content": "42",
                "expected": "42\n"
            },
            {
                "name": "parses booleans",
                "content": "#t",
                "expected": "#t\n"
            },
        ],
        "Advanced Parsers": [
            {
                "name": "handles nested expressions",
                "content": "((lambda (x) x) 42)",
                "expected": "42\n"
            },
            {
                "name": "handles complex define with nested lambda",
                "content": "(define (add a b) (+ a b)) (add 2 3)",
                "expected": "5\n"
            },
        ],
    }
}

# Binary name
binary = "./run-lisp"

# Function to run a single test that does like "./run-lisp < test_file"
def run_test(test_data):
    with tempfile.NamedTemporaryFile("w", delete=True) as temp_file:
        temp_file.write(test_data["content"])
        temp_file.flush()
        try:
            result = subprocess.run(
                [binary],
                input=open(temp_file.name, "r").read(),
                text=True,
                capture_output=True,
            )
            expected_exit_code = test_data.get("exit_code", 0)
            if result.returncode != expected_exit_code:
                raise AssertionError(
                    f"Expected exit code {expected_exit_code}, got {result.returncode}"
                )
            if result.stdout.strip() != test_data["expected"].strip():
                raise AssertionError(
                    f"{colored('expected:', 'red')} {test_data['expected'].strip()}\n"
                    f"{colored('but got:', 'green')} {result.stdout.strip()}"
                )
            return True
        except AssertionError as e:
            return str(e)
        except Exception as ex:
            return f"ERROR: {ex}"

# Function to display test results
def display_results(subcategory_name, tests):
    print(f"  {subcategory_name}")
    for test in tests:
        result = run_test(test)
        if result is True:
            print(f"    {test['name']} [{colored('✔', 'green')}]")
        else:
            print(f"    {test['name']} [{colored('✘', 'red')}]")
            print(f"      {result}")

# Main function
def run_all_tests():
    for category, subcategories in test_categories.items():
        print(f"{category}")
        for subcategory, tests in subcategories.items():
            display_results(subcategory, tests)

if __name__ == "__main__":
    run_all_tests()
