import java.io.*;
import java.util.*;
import java.util.regex.*;

public class SqlTokenAnalyzer {

    // Method to analyze the SQL queries from the CSV file and write the tokens to another CSV
    public static void analyzeCsv(String csvFilePath, String outputCsvFilePath) {
        try {
            BufferedReader csvReader = new BufferedReader(new FileReader(csvFilePath));
            FileWriter csvWriter = new FileWriter(outputCsvFilePath);
            
            String row;
            boolean isFirstRow = true;

            // Write the header of the output CSV
            csvWriter.append("fileName,Token\n");

            // Pattern to find tokens starting with '@' and stopping at '.', '}', ';', '@', ')', '"', or just "_prefix"
            Pattern tokenPattern = Pattern.compile("@[a-zA-Z0-9_]+(?:_prefix)?");

            // Process each row of the CSV
            while ((row = csvReader.readLine()) != null) {
                // Skip the first row, which contains the headers
                if (isFirstRow) {
                    isFirstRow = false;
                    continue;
                }

                // Split the row into fields, assuming fields are quote-delimited and separated by commas
                String[] columns = row.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"); // Handles commas within quoted fields
                if (columns.length < 4) continue; // Ensure the row has all 4 columns: fileName, startLine, endLine, and sqlQuery

                String fileName = columns[0].replace("\"", "");  // Remove quotes around the file name
                String sqlQuery = columns[3].replace("\"", "");  // Remove quotes around the SQL query

                // Find and extract tokens starting with '@'
                Matcher matcher = tokenPattern.matcher(sqlQuery);

                // Write results to the output CSV, one row per token
                while (matcher.find()) {
                    String token = matcher.group(); // Capture the token

                    // Write filename and token in separate columns
                    csvWriter.append(fileName).append(",").append(token).append("\n");
                }
            }

            csvReader.close();
            csvWriter.flush();
            csvWriter.close();

            System.out.println("Token extraction complete. Results saved to: " + outputCsvFilePath);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: java SqlTokenAnalyzer <input_csv_file_path> <output_csv_file_path>");
            return;
        }

        String inputCsvFilePath = args[0];
        String outputCsvFilePath = args[1];

        analyzeCsv(inputCsvFilePath, outputCsvFilePath);
    }
}
