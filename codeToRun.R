library(ConditionCounter)

# Connect to Eunomia

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
connection <- DatabaseConnector::connect(connectionDetails)

# Extract patient counts

occurrences <- extractPatients(connection, addNames = TRUE)

# Plot (example) condition occurrences by year

plotTrend(occurrences = occurrences,
          byMonth = FALSE,
          conditionConceptId = 28060)

# Plot occurrences by month & name

plotTrend(occurrences = occurrences,
          byMonth = TRUE,
          conditionName = "Otitis media")


# Launch app

launchShinyApp()
DatabaseConnector::disconnect(connection)

