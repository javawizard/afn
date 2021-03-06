/*
 * Copyright 2004-2009 H2 Group. Multiple-Licensed under the H2 License,
 * Version 1.0, and under the Eclipse Public License, Version 1.0
 * (http://h2database.com/html/license.html).
 * Initial Developer: H2 Group
 */
package org.h2.api;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * A user-defined aggregate function needs to implement this interface.
 * The class must be public and must have a public non-argument constructor.
 */
public interface AggregateFunction {

    /**
     * This method is called when the aggregate function is used.
     * A new object is created for each invocation.
     *
     * @param conn a connection to the database
     */
    void init(Connection conn) throws SQLException;

    /**
     * This method must return the SQL type of the method,
     * given the SQL type of the input data.
     * The method should check here if the number of parameters passed is correct,
     * and if not it should throw an exception.
     *
     * @param inputTypes the SQL type of the parameters
     * @return the SQL type of the result
     */
    int getType(int[] inputTypes) throws SQLException;

    /**
     * This method is called once for each row.
     * If the aggregate function is called with multiple parameters,
     * those are passed as array.
     *
     * @param value the value(s) for this row
     */
    void add(Object value) throws SQLException;

    /**
     * This method returns the computed aggregate value.
     *
     * @return the aggregated value
     */
    Object getResult() throws SQLException;
}
