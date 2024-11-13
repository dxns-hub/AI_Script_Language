
import os
import subprocess
import json
from datetime import datetime

class COBOLTestRunner:
    def __init__(self):
        self.test_results = {
            'timestamp': datetime.now().isoformat(),
            'tests': [],
            'summary': {
                'total': 0,
                'passed': 0,
                'failed': 0
            }
        }

    def compile_test(self, test_file):
        try:
            subprocess.run(['cobc', '-x', '-o', 'test_suite', test_file], 
                         check=True, capture_output=True)
            return True
        except subprocess.CalledProcessError as e:
            self.test_results['tests'].append({
                'name': test_file,
                'status': 'COMPILE_ERROR',
                'error': e.stderr.decode()
            })
            return False

    def run_tests(self):
        try:
            result = subprocess.run(['./test_suite'], 
                                 capture_output=True, text=True)
            self.parse_test_output(result.stdout)
        except Exception as e:
            self.test_results['tests'].append({
                'name': 'test_suite',
                'status': 'RUNTIME_ERROR',
                'error': str(e)
            })

    def parse_test_output(self, output):
        for line in output.split('\n'):
            if 'Case' in line and (':' in line):
                test_name = line.split(':')[1].strip()
                self.test_results['tests'].append({
                    'name': test_name,
                    'output': []
                })
            elif 'PASSED' in line or 'FAILED' in line:
                if self.test_results['tests']:
                    self.test_results['tests'][-1]['output'].append(line.strip())
                    self.test_results['tests'][-1]['status'] = 'PASSED' if 'PASSED' in line else 'FAILED'

    def save_results(self):
        with open('test_results.json', 'w') as f:
            json.dump(self.test_results, f, indent=2)

        # Create HTML report
        html_report = f'''
        <html>
        <head>
            <title>COBOL Test Results</title>
            <style>
                body {{ font-family: Arial, sans-serif; margin: 20px; }}
                .passed {{ color: green; }}
                .failed {{ color: red; }}
                .test-case {{ margin: 10px 0; padding: 10px; border: 1px solid #ccc; }}
            </style>
        </head>
        <body>
            <h1>COBOL Test Results</h1>
            <p>Timestamp: {self.test_results['timestamp']}</p>
            <div class="test-cases">
        '''
        
        for test in self.test_results['tests']:
            status_class = 'passed' if test['status'] == 'PASSED' else 'failed'
            html_report += f'''
                <div class="test-case">
                    <h3>{test['name']}</h3>
                    <p class="{status_class}">Status: {test['status']}</p>
                    {'<br>'.join(test.get('output', []))}
                </div>
            '''

        html_report += '''
            </div>
        </body>
        </html>
        '''

        with open('test_results.html', 'w') as f:
            f.write(html_report)

# Create and run tests
runner = COBOLTestRunner()
if runner.compile_test('customer_report_test_suite.cbl'):
    runner.run_tests()
    runner.save_results()
    print("Comprehensive test suite created and executed.")
    print("Test results saved to test_results.json and test_results.html")
