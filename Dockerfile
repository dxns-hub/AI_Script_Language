
# Use an official Ubuntu as a parent image
FROM ubuntu:latest

# Set the working directory
WORKDIR /usr/src/app

# Install GNU COBOL
RUN apt-get update &&     apt-get install -y gnucobol

# Copy the current directory contents into the container at /usr/src/app
COPY . .

# Compile the COBOL test suite
RUN cobc -x -o test_suite_coverage customer_report_test_suite_coverage.cbl

# Run the test suite
CMD ["./test_suite_coverage"]
