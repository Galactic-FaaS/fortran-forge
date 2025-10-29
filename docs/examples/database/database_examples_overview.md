# Database Examples

This section demonstrates ForGE Qt's database integration capabilities, including SQL queries, data models, and database management.

## SQL Query Execution

### Basic SQL Queries

**File:** `examples/database_sql_queries/database_sql_queries.f90`

Demonstrates:
- Database connection management
- Executing SELECT, INSERT, UPDATE, DELETE
- Query result processing
- Prepared statements

```fortran
type(forge_database) :: db
type(forge_sql_query) :: query

call db%set_driver("QSQLITE")
call db%open()

call query%prepare("SELECT * FROM users WHERE age > ?")
call query%add_bind_value(21)
call query%execute()
```

## CRUD Operations

### Complete CRUD Implementation

**File:** `examples/database_crud_operations/database_crud_operations.f90`

Demonstrates:
- Create: Inserting new records
- Read: Retrieving data with queries
- Update: Modifying existing records
- Delete: Removing records

## Database Architecture

ForGE Qt's database system includes:

1. **Database Drivers:** SQLite, MySQL, PostgreSQL, etc.
2. **Connection Management:** Connection pooling and management
3. **Query Execution:** Prepared statements and batch operations
4. **Result Processing:** Data retrieval and manipulation
5. **Transaction Support:** ACID compliance

### Connection Setup

```fortran
type(forge_database) :: db
call db%set_host_name("localhost")
call db%set_database_name("myapp")
call db%set_user_name("user")
call db%set_password("password")
call db%open()
```

### Query Execution

```fortran
type(forge_sql_query) :: query
call query%prepare("INSERT INTO users (name, email) VALUES (?, ?)")
call query%add_bind_value("John Doe")
call query%add_bind_value("john@example.com")
call query%execute()
```

## Data Models

ForGE Qt provides data model abstractions:

- **QSqlTableModel:** Table-based data access
- **QSqlQueryModel:** Query-based data access
- **QSqlRelationalTableModel:** Relational data handling

## Transaction Management

```fortran
call db%transaction()
! Execute multiple queries
call db%commit()  ! or db%rollback() on error
```

## Performance Optimization

1. **Prepared Statements:** Reuse query plans
2. **Connection Pooling:** Efficient connection reuse
3. **Batch Operations:** Multiple operations in one call
4. **Indexing:** Proper database schema design

## Error Handling

Database operations can fail due to:
- Connection issues
- SQL syntax errors
- Constraint violations
- Permission problems

```fortran
call query%execute()
if (query%last_error() %is_valid()) then
    print *, "Database error:", query%last_error() %text()
end if
```

## Running Database Examples

```bash
# Build and run database examples
cmake --build build --target database_sql_queries
./build/examples/database_sql_queries/database_sql_queries

# Test CRUD operations
cmake --build build --target database_crud_operations
./build/examples/database_crud_operations/database_crud_operations
```

## Database Features

| Feature | Examples | Description |
|---------|----------|-------------|
| **Queries** | SQL Queries | Basic query execution |
| **CRUD** | CRUD Operations | Data manipulation |
| **Models** | All examples | Data abstraction |
| **Transactions** | All examples | Atomic operations |

## Best Practices

1. **Connection Management:** Use connection pooling
2. **Prepared Statements:** Prevent SQL injection
3. **Error Handling:** Check all database operations
4. **Transactions:** Use for data consistency
5. **Resource Cleanup:** Close connections properly

## Supported Databases

ForGE Qt supports multiple database backends:
- **SQLite:** Embedded database
- **MySQL/MariaDB:** Popular open-source RDBMS
- **PostgreSQL:** Advanced open-source RDBMS
- **Oracle:** Enterprise RDBMS
- **ODBC:** Generic database access

These examples provide comprehensive database integration patterns for ForGE Qt applications.