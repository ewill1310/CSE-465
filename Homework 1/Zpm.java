
import java.io.File;
import java.io.IOException;

public class Zpm {

    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("No file given.");
            return;
        }

        String filePath = args[0];

        try {
            validateFile(filePath);
        } catch (IOException e) {
            System.err.println(e.getMessage());
            return;
        }

        try {
            File file = new File(filePath);
            Interpreter zpmFile = new Interpreter(file);
        } catch (Exception e) {
            System.err.println("Cannot open file: " + e.getMessage());
            System.exit(0);
        }
    }

    public static void validateFile(String filePath) throws IOException {
        if (filePath == null || filePath.isEmpty()) {
            throw new IOException("No file provided.");
        }

        File file = new File(filePath);

        if (!file.exists()) {
            throw new IOException("File does not exist.");
        }

        if (!file.isFile()) {
            throw new IOException("The path does not point to a file.");
        }

        if (!file.getName().endsWith(".zpm")) {
            throw new IOException("The file is not a .zpm file.");
        }
    }
}
