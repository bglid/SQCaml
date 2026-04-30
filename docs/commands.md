# Current working commands:

*Note, all commands are ended by a* `;`
*All commands are case sensitive*
- - - 
## INSERT

##### Inserts data into storage engine

To insert data, start a command with INSERT, like:
```sql
-- INSERT INTO *table* (*fields, fields...*)
INSERT INTO ... (..., ..., ...)
-- VALUES (*data to insert*)
VALUES (...);
```
 
 - Currently this is a fixed table schema of (id, stop_name, rail_line)


##### Example
```sql
-- Sample insert
INSERT INTO Mbta (id, stop_name, rail_line)
VALUES (100, 'Englewood ave.', 'G');
```

 - Commands don't need newlines to run, so the above example could also be run like:
```sql
-- Sample insert
INSERT INTO Mbta (id, stop_name, rail_line) VALUES (100, 'Englewood ave.', 'G');
```


## SELECT

##### Queries data from the storage engine

To query data, start a command with SELECT, like:
```sql
-- SELECT FROM *table* (*fields, fields...*)
SELECT FROM ... (...);
```

##### Example
```sql
-- Sample query 
SELECT FROM Mbta (stop_name);  -- will return all stop names in db
```

#### Predicates

Currently a subset of predicates are supported:
 - Equals: `==`
 - Not equal: `<>`
 - greater than, less than, etc. : `>`, `>=`, `<`, `<=`


```sql
-- SELECT FROM *table* (*fields...*) WHERE predicate...
SELECT FROM ... (...)
WHERE ... == ...; 
```

##### Example
```sql
-- Sample query 
SELECT FROM Mbta (stop_name)
WHERE rail_line <> 'G';  -- Returns all stops not on the greenline (thank god)
```
- - -
## Meta commands

-  `.help;` -> Prints out help menu
-  `.tree;` -> Prints out current tree layout and ids
-  `.exit;` -> Exits SQCaml

- - -
That's all I have for now!
🐫︎
