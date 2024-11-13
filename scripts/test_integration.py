
import os
import re
from datetime import datetime

def run_cobol_tests(test_file):
    '''Run COBOL tests and return results'''
    try:
        # Compile test file
        os.system(f'cobc -x -o test_program {test_file}')
        # Run tests
        test_output = os.popen('./test_program').read()
        return test_output
    except Exception as e:
        return f"Error running tests: {str(e)}"

def update_test_documentation(md_file, test_results):
    '''Update markdown documentation with test results'''
    with open(md_file, 'a') as f:
        f.write('\n## Test Results\n')
        f.write('```\n')
        f.write(test_results)
        f.write('\n```\n')

# Add test results to existing documentation
if os.path.exists('customer_report_test.cbl'):
    test_results = run_cobol_tests('customer_report_test.cbl')
    update_test_documentation('documentation.md', test_results)
