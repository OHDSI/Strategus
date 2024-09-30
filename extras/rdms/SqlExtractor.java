import java.io.*;
import java.nio.file.*;
import java.util.regex.*;
import java.util.List;
import java.util.ArrayList;

public class SqlExtractor {

    // Method to scan a directory and process all .R files
    public static void scanDirectory(String directoryPath, String outputCsvPath) {
        try {
            // Collect all .R files in the directory
            List<File> rFiles = getRFiles(directoryPath);

            // Create a list to store SQL extractions
            List<String[]> extractedData = new ArrayList<>();
            int totalSqlExcerpts = 0;

            // Process each R file
            for (File file : rFiles) {
                List<String[]> sqlQueries = extractSqlFromFile(file);
                extractedData.addAll(sqlQueries);
                totalSqlExcerpts += sqlQueries.size();
                // Output the number of SQL queries found in each file
                System.out.println(file.getName() + " - " + sqlQueries.size() + " SQL statements found");
            }

            // Write extracted SQL to CSV
            writeCsv(outputCsvPath, extractedData);

            System.out.println("Extraction complete. Total SQL excerpts extracted: " + totalSqlExcerpts);
            System.out.println("Results saved to: " + outputCsvPath);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Method to get all .R files from a directory
    private static List<File> getRFiles(String directoryPath) throws IOException {
        List<File> rFiles = new ArrayList<>();
        Files.walk(Paths.get(directoryPath))
            .filter(Files::isRegularFile)
            .filter(path -> path.toString().endsWith(".R"))
            .forEach(path -> rFiles.add(path.toFile()));
        return rFiles;
    }

    // Method to extract SQL queries from a file, including start and end line numbers
    private static List<String[]> extractSqlFromFile(File file) throws IOException {
        List<String[]> sqlQueries = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line;
        boolean capturingSql = false;
        StringBuilder sqlBuilder = new StringBuilder();
        char quoteChar = 0;  // To store the opening quote character (' or ")
        int lineNumber = 0;
        int startLineNumber = 0;

        // Regex to detect the start of the SQL query: `sql <- "..."` or `sql <- '...'`
        Pattern startPattern = Pattern.compile("sql\\s*<-\\s*(['\"])(.*)"); // Detects start with either single or double quotes

        while ((line = reader.readLine()) != null) {
            lineNumber++; // Increment the line number for each line read
            if (capturingSql) {
                // Continue capturing until the matching quote character is found
                int closingQuoteIndex = line.indexOf(quoteChar);
                if (closingQuoteIndex != -1) {
                    sqlBuilder.append(line, 0, closingQuoteIndex); // Capture the SQL until the closing quote
                    sqlQueries.add(new String[]{
                        "\"" + file.getName() + "\"", 
                        "\"" + startLineNumber + "\"", 
                        "\"" + lineNumber + "\"", 
                        "\"" + sqlBuilder.toString().trim() + "\""
                    });  // Save the captured SQL
                    capturingSql = false;  // Stop capturing
                } else {
                    sqlBuilder.append(line.trim()).append(" "); // Continue capturing across lines
                }
            } else {
                // Check if the line contains the start of the SQL query
                Matcher startMatcher = startPattern.matcher(line);
                if (startMatcher.find()) {
                    quoteChar = startMatcher.group(1).charAt(0);  // Capture the opening quote character (either ' or ")
                    sqlBuilder.setLength(0); // Clear the builder
                    sqlBuilder.append(startMatcher.group(2)); // Start capturing after the opening quote
                    startLineNumber = lineNumber; // Set the line number where SQL starts

                    // Check if the SQL ends on the same line
                    int closingQuoteIndex = sqlBuilder.indexOf(Character.toString(quoteChar));
                    if (closingQuoteIndex != -1) {
                        sqlQueries.add(new String[]{
                            "\"" + file.getName() + "\"", 
                            "\"" + startLineNumber + "\"", 
                            "\"" + lineNumber + "\"", 
                            "\"" + sqlBuilder.substring(0, closingQuoteIndex).trim() + "\""
                        }); // Capture the SQL before the closing quote
                        capturingSql = false;
                    } else {
                        capturingSql = true; // SQL spans multiple lines
                    }
                }
            }
        }
        reader.close();
        return sqlQueries;
    }

    // Method to write the extracted SQL queries to a CSV file with start and end line numbers
    private static void writeCsv(String csvPath, List<String[]> data) throws IOException {
        FileWriter csvWriter = new FileWriter(csvPath);
        // Update column headings to exactly match your request
        csvWriter.append("\"fileName\",\"startLine\",\"endLine\",\"sqlQuery\"\n");

        for (String[] row : data) {
            csvWriter.append(String.join(",", row)).append("\n");
        }
        csvWriter.flush();
        csvWriter.close();
    }

    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: java SqlExtractor <directory_path> <output_csv_path>");
            return;
        }

        String directoryPath = args[0];
        String outputCsvPath = args[1];

        scanDirectory(directoryPath, outputCsvPath);
    }
}
