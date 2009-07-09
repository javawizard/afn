/*
 * This file implements an ibatis-like interface for persistent data storage.
 * JZBot provides a persistent data store to the scripts in the form of a JDBC
 * connection connected to an H2 database. This script interfaces with it.
 * 
 * The script benefits over ibatis from the fact that objects in javascript have
 * no formal class, and can therefore contain any properties they want. This
 * makes it so that the functions defined in this file don't need to take the
 * name of a class to return; instead, they just create new objects and assign
 * each column to a property within the object.
 */

if ((typeof persistence) == "undefined")
{
	Global.persistence = new Object();
}
/**
 * Returns an array of objects, one object for each result from the query, with
 * all properties in the object set to columns returned from the query.
 */
persistence.selectList = function(query, parameter)
{
	var set = this.p_parseParameters(query);
	var statement = jzbot.persistent.prepareStatement(set.query);
	for ( var i = 0; i < set.values.length; i++)
	{
		statement.setObject(i + 1, parameter[set.values[i]]);
	}
	var resultSet = statement.executeQuery();
	var index = 0;
	var results = [];
	var metadata = resultSet.getMetaData();
	var columnCount = metadata.getColumnCount();
	var columnNames = [];
	for ( var i = 0; i < columnCount; i++)
	{
		columnNames[i] = metadata.getColumnName(i + 1);
	}
	while (resultSet.next())
	{
		var result = new Object();
		for ( var i = 0; i < columnNames.length; i++)
		{
			result[columnNames[i]] = resultSet.getObject(i+1);
		}
		results[index++] = result;
	}
	resultSet.close();
	statement.close();
	return results;
};
/**
 * Same as selectList, but expects only one column from the query and returns a
 * list of the values in that column. For example, "select col from mytable",
 * with col being a column of type int, would return a list of ints, not a list
 * of objects with a "col" property like selectList would return.
 */
persistence.selectSingleList = function(query, parameter)
{
	var original = this.selectList(query, parameter);
	var results = [];
	for ( var i = 0; i < original.length; i++)
	{
		var result = null;
		for ( var col in original[i])
		{
			result = col;
			break;
		}
		results[i] = result;
	}
	return results;
};
/**
 * Same as selectList, but expects 0 or 1 rows returned from the query, and
 * returns null or the result, respectively.
 */
persistence.selectObject = function(query, parameter)
{
	var result = this.selectList(query, parameter);
	if (result.length == 0)
		return null;
	return result[0];
};
/**
 * Same as selectSingleList, but expects 0 or 1 rows returned from the query,
 * and returns null or the result, respectively.
 */
persistence.selectSingleObject = function(query, parameter)
{
	var result = this.selectSingleList(query, parameter);
	if (result.length == 0)
		return null;
	return result[0];
};
/**
 * Performs the specified query without returning anything. This should be used
 * for updates.
 */
persistence.update = function(query, parameter)
{
	var set = this.p_parseParameters(query);
	var statement = jzbot.persistent.prepareStatement(set.query);
	for ( var i = 0; i < set.values.length; i++)
	{
		statement.setObject(i + 1, parameter[set.values[i]]);
	}
	statement.executeUpdate();
	statement.close();
};
/**
 * Takes one parameter, query, which is the query to parse. Replaces all #...#
 * with ?, and returns an object with two properties: query, which is the new
 * query with the replacements just described, and values, which is a list of
 * the values of the #...# sequences. For example, "select * from test where
 * col1 = #firstvalue# and col2 = #secondvalue#" would cause {query:"select *
 * from test where col1 = ? and col2 = ?",values=["firstvalue","secondvalue"]}
 * to be returned.
 */
persistence.p_parseParameters = function(query)
{
	var values = [];
	var items = query.split('#');
	if ((items.length % 2) != 1)
		throw "Unmatched # sign in query";
	var newQuery = items[0];
	for ( var i = 1; i < items.length; i++)
	{
		if ((i % 2) == 1)
		{
			// We're in a # value
			values[(i - 1) % 2] = items[i];
			newQuery += "?";
		}
		else
		{
			// We're in a literal query string
			newQuery += items[i];
		}
	}
	var result =
	{
		"query" :newQuery,
		"values" :values
	};
	return result;
}

/*
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 */