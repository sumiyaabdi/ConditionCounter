library(ConditionCounter)

# Connect to Eunomia

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
connection <- DatabaseConnector::connect(connectionDetails)

# Extract patient counts

occurrences <- extractPatients(connection)

# Plot (example) condition occurrences by year

plotTrend(occurrences = occurrences,
          byMonth = FALSE,
          conditionName = "Otitis media")

# Plot occurrences by month

plotTrend(occurrences = occurrences,
          byMonth = TRUE,
          conditionName = "Otitis media")


# Launch app

launchShinyApp()
DatabaseConnector::disconnect(connection)

