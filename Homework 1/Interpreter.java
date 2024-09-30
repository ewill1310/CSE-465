import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class Interpreter {
    private Scanner fileScanner;
    private Map<String, Object> variables;
    private File sourceFile;
    private int currentLine = 0;

    public Interpreter(File file) {
        variables = new HashMap<>();
        this.sourceFile = file;
        try {
            fileScanner = new Scanner(sourceFile);
        } catch (Exception e) {
            System.out.println("Error: Cannot open file.");
            System.exit(1);
        }
        processFile();
    }

    private void processFile() {
        while (fileScanner.hasNext()) {
            String line = fileScanner.nextLine().trim();
            currentLine++;
            interpretLine(line);
        }
    }

    private void interpretLine(String line) {
        if (line.contains("PRINT")) {
            handlePrint(line);
        } else if (line.contains("FOR")) {
            handleForLoop(line);
        } else if (line.contains("+=")) {
            handleAdditionAssignment(line);
        } else if (line.contains("*=")) {
            handleMultiplicationAssignment(line);
        } else if (line.contains("-=")) {
            handleSubtractionAssignment(line);
        } else if (line.contains("=")) {
            handleAssignment(line);
        }
    }

    private void handlePrint(String line) {
        String[] parts = line.split(" ");
        String variableName = parts[1].replace(";", "");
        Object value = variables.get(variableName);
        if (value == null) {
            System.out.println("RUNTIME ERROR: variable " + variableName + " not initialized");
            System.exit(1);
        }
        System.out.println(variableName + "=" + value);
    }

    private void handleForLoop(String line) {
        try {
            String[] parts = line.split(" ");
            int iterations = Integer.parseInt(parts[1].trim());
            String loopBody = line.substring(line.indexOf(" ", 4)).trim();
            loopBody = loopBody.substring(0, loopBody.lastIndexOf("ENDFOR")).trim();

            for (int i = 0; i < iterations; i++) {
                String[] statements = loopBody.split(";");
                for (String statement : statements) {
                    if (!statement.trim().isEmpty()) {
                        interpretLine(statement.trim());
                    }
                }
            }
        } catch (Exception e) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }
    }

    private void handleAssignment(String line) {
        String[] parts = line.split(" ");
        try {
            String variableName = parts[0];
            String value = parts[2].replace(";", "").trim();
            if (value.startsWith("\"") && value.endsWith("\"")) {
                value = value.substring(1, value.length() - 1);
            }
            variables.put(variableName, parseValue(value));
        } catch (Exception e) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }
    }

    private Object parseValue(String value) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return value;
        }
    }

    private void handleAdditionAssignment(String line) {
        String[] parts = line.split("\\+=");
        if (parts.length != 2) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }

        String variableName = parts[0].trim();
        String value = parts[1].replace(";", "").trim();

        Object currentValue = variables.get(variableName);
        
        if (currentValue == null) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }

        if (currentValue instanceof String) {
            String newValue = (String) currentValue;
            if (variables.containsKey(value) && variables.get(value) instanceof String) {
                newValue += variables.get(value);
            } else if (value.startsWith("\"") && value.endsWith("\"")) {
                newValue += value.substring(1, value.length() - 1);
            } else {
                System.out.println("RUNTIME ERROR: line " + currentLine);
                System.exit(1);
            }

            variables.put(variableName, newValue);
        } else if (currentValue instanceof Integer) {
            try {
                int valueToAdd = getIntValue(value);
                variables.put(variableName, (Integer) currentValue + valueToAdd);
            } catch (NumberFormatException e) {
                System.out.println("RUNTIME ERROR: line " + currentLine);
                System.exit(1);
            }
        } else {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }
    }



    private int parseIntegerValue(String value) {
        try {
            if (variables.containsKey(value) && variables.get(value) instanceof Integer) {
                return (Integer) variables.get(value);
            } else {
                return Integer.parseInt(value);
            }
        } catch (NumberFormatException e) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
            return 0;
        }
    }

    private String parseStringValue(String value) {
        if (value.startsWith("\"") && value.endsWith("\"")) {
            return value.substring(1, value.length() - 1);
        }
        return value;
    }

    private void handleMultiplicationAssignment(String line) {
        String[] parts = line.split(" ");
        try {
            String variableName = parts[0];
            String value = parts[2].replace(";", "").trim();
            int currentValue = (Integer) variables.get(variableName);
            int multiplyValue = parseIntegerValue(value);
            variables.put(variableName, currentValue * multiplyValue);
        } catch (Exception e) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }
    }

    private void handleSubtractionAssignment(String line) {
        String[] parts = line.split(" ");
        try {
            String variableName = parts[0];
            String value = parts[2].replace(";", "").trim();
            int currentValue = (Integer) variables.get(variableName);
            int subtractValue = parseIntegerValue(value);
            variables.put(variableName, currentValue - subtractValue);
        } catch (Exception e) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }
    }

    private Integer getIntValue(String value) {
        // Check if the value is a variable
        if (variables.containsKey(value)) {
            Object varValue = variables.get(value);
            // Ensure that the variable is an integer
            if (varValue instanceof Integer) {
                return (Integer) varValue;
            } else {
                System.out.println("RUNTIME ERROR: line " + currentLine);
                System.exit(1);
            }
        }
        // Attempt to parse it as an integer
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            System.out.println("RUNTIME ERROR: line " + currentLine);
            System.exit(1);
        }
        return null; // This line will never be reached due to System.exit above
    }

}
