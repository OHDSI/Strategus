import java.io.*;
import java.util.*;
import java.util.regex.*;

public class SqlJoinRelationshipExtractor {

    // Method to analyze SQL queries and extract the FROM and JOIN relationships, including multiple conditions (AND/OR)
    public static void extractSqlJoins(String inputCsvFilePath, String outputCsvFilePath) {
        try {
            BufferedReader csvReader = new BufferedReader(new FileReader(inputCsvFilePath));
            FileWriter csvWriter = new FileWriter(outputCsvFilePath);

            String row;
            boolean isFirstRow = true;

            // Write the header of the output CSV
            csvWriter.append("fileName,startLine,endLine,fromClause,joinRelationship\n");

            // Pattern to match FROM clause
            Pattern fromPattern = Pattern.compile("from\\s+([\\w.@]+)\\s+([\\w@]+)", Pattern.CASE_INSENSITIVE);
            // Pattern to match JOIN clauses, including AND/OR conditions
            Pattern joinPattern = Pattern.compile("join\\s+([\\w.@]+)\\s+([\\w@]+)\\s+on\\s+(.+?)(?=\\s+join|\\s+where|;|$)", Pattern.CASE_INSENSITIVE);

            // Process each row of the CSV (from SqlExtractor)
            while ((row = csvReader.readLine()) != null) {
                if (isFirstRow) {
                    isFirstRow = false; // Skip the header
                    continue;
                }

                // Split the row into columns (fileName, startLine, endLine, sqlQuery)
                String[] columns = row.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (columns.length < 4) continue; // Ensure row has the necessary columns

                String fileName = columns[0].replace("\"", ""); // Extract fileName
                String startLine = columns[1].replace("\"", ""); // Extract startLine
                String endLine = columns[2].replace("\"", ""); // Extract endLine
                String sqlQuery = columns[3].replace("\"", ""); // Extract the SQL query

                // Capture the FROM clause
                String fromClause = "";
                Matcher fromMatcher = fromPattern.matcher(sqlQuery);
                if (fromMatcher.find()) {
                    fromClause = "FROM " + fromMatcher.group(1) + " " + fromMatcher.group(2);
                }

                // Extract JOIN clauses including AND/OR conditions
                Matcher joinMatcher = joinPattern.matcher(sqlQuery);
                while (joinMatcher.find()) {
                    // Capture the full JOIN clause
                    String fullJoinClause = "JOIN " + joinMatcher.group(1) + " " + joinMatcher.group(2) + " ON " + joinMatcher.group(3).trim();

                    // Split the join conditions by AND/OR (but keep AND/OR in the condition for clarity)
                    String[] joinConditions = joinMatcher.group(3).split("(?<=\\))\\s+(AND|OR)\\s+");

                    // For each condition in the JOIN clause, create a new row
                    for (String condition : joinConditions) {
                        String joinRelationship = "JOIN " + joinMatcher.group(1) + " " + joinMatcher.group(2) + " ON " + condition.trim();

                        // Write fileName, startLine, endLine, fromClause, and each joinRelationship to the CSV
                        csvWriter.append(fileName).append(",")
                                 .append(startLine).append(",")
                                 .append(endLine).append(",")
                                 .append("\"").append(fromClause).append("\",")
                                 .append("\"").append(joinRelationship).append("\"\n");
                    }
                }
            }

            csvReader.close();
            csvWriter.flush();
            csvWriter.close();

            System.out.println("SQL FROM and JOIN relationships extraction complete. Results saved to: " + outputCsvFilePath);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: java SqlJoinRelationshipExtractor <input_csv_file_path> <output_csv_file_path>");
            return;
        }

        String inputCsvFilePath = args[0];
        String outputCsvFilePath = args[1];

        extractSqlJoins(inputCsvFilePath, outputCsvFilePath);
    }
}
