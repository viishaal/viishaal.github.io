library(RSQLite)

db = dbConnect(dbDriver("SQLite"), "../data/input/database.sqlite")

emails = dbGetQuery(db, "
                     SELECT ExtractedBodyText body,MetadataSubject subject, MetadataDateSent date 
                     FROM Emails e 
                     INNER JOIN Persons p 
                     ON e.SenderPersonId=P.Id 
                     WHERE p.Name='Hillary Clinton'  
                     ORDER BY RANDOM()")