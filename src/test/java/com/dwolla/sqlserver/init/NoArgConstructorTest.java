package com.dwolla.sqlserver.init;

/**
 * If this compiles, Lambda should be able to construct an instance
 * of SqlServerDatabaseInitHandler without any constructor arguments.
 */
public class NoArgConstructorTest {
    public static SqlServerDatabaseInitHandler handler = new SqlServerDatabaseInitHandler();
}
