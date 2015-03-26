package com.cristakislab.trellissync;

import java.io.OutputStream;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.List;
import java.util.Map;

import org.apache.commons.dbutils.QueryRunner;
import org.apache.commons.dbutils.handlers.MapListHandler;
import org.apache.commons.dbutils.handlers.ScalarHandler;

public class SurveySync {

	private Connection conn;
	private Long serverId;
	private List<Long> serverIds; // TODO: for central!

	public SurveySync(Connection conn) {
		this.conn = conn;
	}

	protected Connection getConn() {
		return conn;
	}

	/**
	 * <p>
	 * Get's current conection's server_id (of course MySQL specific).
	 * </p>
	 * 
	 * @return
	 * @throws SQLException
	 */
	protected Long getServerId() throws SQLException {
		if (serverId == null) {
			ScalarHandler<Number> handler = new ScalarHandler<Number>();
			QueryRunner runner = new QueryRunner();
			Number result = (Number) runner.query(getConn(),
					"select @@server_id", handler);
			serverId = result.longValue();
		}
		return serverId;
	}

	/**
	 * <p>
	 * Gets the records from <code>tableName</code> where
	 * <code>modify_date > lastModifyDate</code>.
	 * </p>
	 * <p>
	 * Is expected to get few hundreds or thousands records say ~10k.
	 * </p>
	 * 
	 * @param tableName
	 * @param lastModifyDate
	 * @return Each record is an element of the list
	 * @throws SQLException
	 */
	public List<Map<String, Object>> getModifiedRecords(String tableName,
			long lastModifyDate, long newModifyDate) throws SQLException {
		checkHasModifyDate(tableName);
		String sql = MessageFormat.format(
				"select * from {0} where modify_date > ? and modify_date <= ?",
				tableName);
		MapListHandler handler = new MapListHandler();
		QueryRunner runner = new QueryRunner();
		return runner.query(getConn(), sql, handler, lastModifyDate,
				newModifyDate);
	}

	protected void saveRecords(List<Map<String, Object>> records,
			OutputStream out) {

	}

	public void getModifiedRecords(List<String> tableNames,
			long lastModifyDate, long newModifyDate) {
		// Write to zip stream?
	}

	/**
	 * <p>
	 * Checks if <code>tableName</code> has <code>columnName</code>. If so
	 * returns <code>true</code> else <code>false</code>.
	 * </p>
	 * 
	 * @param tableName
	 * @param columnName
	 * @return
	 * @throws SQLException
	 */
	protected boolean hasColumn(String tableName, String columnName)
			throws SQLException {
		DatabaseMetaData dbmd = getConn().getMetaData();
		ResultSet rst = dbmd.getColumns(null, null, tableName, columnName);
		boolean result = rst.first();
		rst.close();
		return result;
	}

	/**
	 * <p>
	 * Checks if <code>tableName</code> has <code>columnName</code> column, if
	 * not throws an <code>IllegalArgumentException</code>.
	 * </p>
	 * 
	 * @param tableName
	 * @param columnName
	 * @throws SQLException
	 */
	protected void checkHasColumn(String tableName, String columnName)
			throws SQLException {
		if (!hasColumn(tableName, columnName)) {
			throw new IllegalArgumentException(tableName + " does not have "
					+ columnName + " column");
		}
	}

	/**
	 * <p>
	 * Checks if <code>tableName</code> has <code>modify_date</code> column.
	 * </p>
	 * 
	 * @param tableName
	 * @return
	 * @throws SQLException
	 */
	protected boolean hasModifyDate(String tableName) throws SQLException {
		return hasColumn(tableName, "modify_date");
	}

	/**
	 * <p>
	 * Checks if <code>tableName</code> has <code>modify_date</code> column, if
	 * not throws an <code>IllegalArgumentException</code>
	 * </p>
	 * 
	 * @param tableName
	 * @throws SQLException
	 */
	protected void checkHasModifyDate(String tableName) throws SQLException {
		checkHasColumn(tableName, "modify_date");
	}

	public void remoteToCentral() {
		// Prepare zip file containing all modified records of all tables
		// zip file: yyyyMMddHHmmss_<server_id>.zip
		// <table>.json
	}
	
	public void processRemote() {
		// Process zip file got from remote. One file at a time.
		// Load sync_in_* tables
		// Process sync_in_* updating central data and generating records for sync_out_*
		// Prepares zip files from sync_out_*, one file per server.
	}
	
}

/*
FileOutputStream fos = null;
try {
    // Make sure that the output stream is in Append mode. Otherwise you will
    // truncate your file, which probably isn't what you want to do :-) 
    fos = new FileOutputStream(file, true);
    // -> file was closed
} catch(IOException e) {
    // -> file still open
} finally {
    if(fos != null) {
    try {
        fos.close();
    } catch (IOException e) {
        e.printStackTrace();
    }
}
*/