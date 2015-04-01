package com.cristakislab.trellissync;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import org.apache.commons.dbutils.QueryRunner;
import org.apache.commons.dbutils.handlers.ArrayListHandler;
import org.apache.commons.dbutils.handlers.MapListHandler;
import org.apache.commons.dbutils.handlers.ScalarHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

// TODO: images! include in zip file?
public class DataSynchronizer {
	private static final Logger logger = LoggerFactory
			.getLogger(DataSynchronizer.class);

	private Connection conn;

	public DataSynchronizer(Connection conn) {
		this.conn = conn;
	}

	protected Connection getConn() {
		return conn;
	}

	private Long serverId;

	/**
	 * <p>
	 * Get's current conection's server_id (MySQL specific).
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
	 * Gets current time in unix format.
	 * </p>
	 * 
	 * @return
	 */
	protected long currentUnixTime() {
		return System.currentTimeMillis() / 1000L;
	}

	private Long lastModifyDate;

	protected String SQL_GET_LAST_MODIFIED_DATE = ""
			+ "select last_modify_date from sync_control limit 1";

	/**
	 * <p>
	 * Returns the last modify date stored in sync_control.
	 * </p>
	 * 
	 * @return
	 * @throws SQLException
	 */
	protected long getLastModifyDate() throws SQLException {
		if (lastModifyDate == null) {
			ScalarHandler<Number> handler = new ScalarHandler<Number>();
			QueryRunner runner = new QueryRunner();
			Number result = (Number) runner.query(getConn(),
					SQL_GET_LAST_MODIFIED_DATE, handler);
			if (result == null) {
				throw new IllegalStateException("sync_control is empty.");
			}
			lastModifyDate = result.longValue();
		}
		return lastModifyDate;
	}

	protected static final String SQL_UPD_LAST_MODIFY_DATE = ""
			+ "update sync_control set last_modify_date = ?";

	protected static final String SQL_INS_LAST_MODIFY_DATE = ""
			+ "insert into sync_control(last_modify_date) values (?)";

	protected void updateLastModifyDate(long newModifyDate) throws SQLException {
		QueryRunner runner = new QueryRunner();
		if (runner.update(getConn(), SQL_UPD_LAST_MODIFY_DATE, newModifyDate) == 0) {
			runner.update(getConn(), SQL_INS_LAST_MODIFY_DATE, newModifyDate);
		}
	}

	private List<String> outboundTables;

	protected static final String SQL_GET_OUTBOND_TABLES = ""
			+ "select table_name from sync_tables where outbound = 'Y'";

	protected List<String> getOutboundTables() throws SQLException {
		if (outboundTables == null) {
			ArrayListHandler handler = new ArrayListHandler();
			QueryRunner runner = new QueryRunner();
			List<Object[]> records = runner.query(getConn(),
					SQL_GET_OUTBOND_TABLES, handler);
			outboundTables = new ArrayList<>();
			records.forEach(o -> outboundTables.add(o[0].toString()));
		}
		return outboundTables;
	}

	private List<String> inboundTables;

	protected static final String SQL_GET_INBOUND_TABLES = ""
			+ "select table_name from sync_tables where inbound = 'Y' order by in_order, table_name";

	protected List<String> getInboundTables() throws SQLException {
		if (inboundTables == null) {
			ArrayListHandler handler = new ArrayListHandler();
			QueryRunner runner = new QueryRunner();
			List<Object[]> records = runner.query(getConn(),
					SQL_GET_INBOUND_TABLES, handler);
			inboundTables = new ArrayList<>();
			records.forEach(o -> inboundTables.add(o[0].toString()));
		}
		return inboundTables;
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
		logger.debug("Checking modified records since {} on {}",
				lastModifyDate, tableName);
		String sql = MessageFormat.format(
				"select * from {0} where modify_date > ? and modify_date <= ?",
				tableName);
		MapListHandler handler = new MapListHandler();
		QueryRunner runner = new QueryRunner();
		return runner.query(getConn(), sql, handler, lastModifyDate,
				newModifyDate);
	}

	private GsonBuilder gsonBuilder;

	protected GsonBuilder getGsonBuilder() {
		if (gsonBuilder == null) {
			gsonBuilder = new GsonBuilder().setDateFormat("yyyy-MM-dd");
		}
		return gsonBuilder;
	}

	protected void saveRecords(List<Map<String, Object>> records,
			OutputStream out) throws IOException {
		Gson gson = getGsonBuilder().create();
		JsonWriter writer = new JsonWriter(new OutputStreamWriter(out, "UTF-8"));
		writer.beginArray();
		records.forEach(r -> gson.toJson(r, Map.class, writer));
		writer.endArray();
		writer.flush();
	}

	protected void addRecordsFile(String tableName,
			List<Map<String, Object>> records, ZipOutputStream zipOut)
			throws IOException {
		ZipEntry entry = new ZipEntry(tableName + ".json");
		zipOut.putNextEntry(entry);
		try {
			saveRecords(records, zipOut);
			logger.info("Saved {} record(s) of table \"{}\".", records.size(),
					tableName);
		} finally {
			zipOut.closeEntry();
		}
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

	protected String getPrimaryKeyField(String tableName) throws SQLException {
		DatabaseMetaData dbmd = getConn().getMetaData();
		ResultSet rst = dbmd.getPrimaryKeys(null, null, tableName);
		if (!rst.next()) {
			throw new IllegalArgumentException(tableName
					+ " does not have primary key.");
		}
		String result = rst.getString("COLUMN_NAME");
		if (rst.next()) {
			throw new IllegalArgumentException(
					tableName
							+ " have composed primary key - we support only one column primary keys.");
		}
		rst.close();
		return result;
	}

	protected List<String> getFieldNames(String tableName) throws SQLException {
		DatabaseMetaData dbmd = getConn().getMetaData();
		ResultSet rst = dbmd.getColumns(null, null, tableName, "%");
		List<String> result = new ArrayList<>();
		while (rst.next()) {
			result.add(rst.getString("COLUMN_NAME"));
		}
		rst.close();
		return result;
	}

	protected void lockTables(List<String> tableNames) throws SQLException {
		StringBuilder sql = new StringBuilder();
		tableNames.forEach(t -> sql.append(',').append(t).append(" write"));
		sql.insert(0, "lock tables sync_control write");
		QueryRunner runner = new QueryRunner();
		runner.update(getConn(), sql.toString());
		logger.info("Tables locked: {}", tableNames);
	}

	protected void unlockTables() throws SQLException {
		QueryRunner runner = new QueryRunner();
		runner.update(getConn(), "unlock tables");
		logger.info("Tables unlocked.");
	}

	protected void addServerIdFile(ZipOutputStream zipOut) throws IOException,
			SQLException {
		ZipEntry entry = new ZipEntry("server.id");
		zipOut.putNextEntry(entry);
		try {
			zipOut.write(getServerId().toString().getBytes());
		} finally {
			zipOut.closeEntry();
		}
	}

	public void addModifiedData(ZipOutputStream zipOut) throws IOException,
			SQLException {
		addServerIdFile(zipOut);
		lockTables(getOutboundTables());
		try {
			long lastModifyDate = getLastModifyDate();
			long newModifyDate = currentUnixTime();
			for (String tableName : getOutboundTables()) {
				List<Map<String, Object>> records = getModifiedRecords(
						tableName, lastModifyDate, newModifyDate);
				if (!records.isEmpty()) {
					addRecordsFile(tableName, records, zipOut);
				}
			}
			updateLastModifyDate(newModifyDate);
		} finally {
			unlockTables();
		}
	}

	public String sendModifiedData(String outputDir) throws IOException,
			SQLException {
		String result = MessageFormat.format(
				"{0}{1,date,yyyyMMdd_HHmmss}_{2,number,0}.zip", outputDir
						+ (outputDir.endsWith("/") ? "" : "/"), new Date(),
				getServerId());
		logger.info("Generated zip file name: {}", result);
		try (OutputStream out = new FileOutputStream(result);
				ZipOutputStream zipOut = new ZipOutputStream(out)) {
			addModifiedData(zipOut);
		}
		return result;
	}

	protected List<Map<String, Object>> loadRecords(InputStream in)
			throws IOException {
		List<Map<String, Object>> result = new ArrayList<>();
		Gson gson = getGsonBuilder().create();
		JsonReader reader = new JsonReader(new InputStreamReader(in, "UTF-8"));
		reader.beginArray();
		while (reader.hasNext()) {
			Map<String, Object> record = gson.fromJson(reader, Map.class);
			result.add(record);
		}
		reader.endArray();
		return result;
	}

	protected PreparedStatement createSeletStatement(String tableName,
			List<String> fieldNames, String primaryKeyField)
			throws SQLException {
		StringBuilder sql = new StringBuilder(512);
		fieldNames.forEach(f -> sql.append(sql.length() == 0 ? "" : ", ")
				.append(f));
		sql.insert(0, "select ").append(" from ").append(tableName)
				.append(" where ").append(primaryKeyField).append(" = ?");
		return getConn().prepareStatement(sql.toString());
	}

	protected PreparedStatement createInsertStatement(String tableName,
			List<String> fieldNames) throws SQLException {
		StringBuilder sql = new StringBuilder(512);
		StringBuilder holders = new StringBuilder(fieldNames.size() * 2);
		fieldNames.forEach(f -> sql.append(sql.length() == 0 ? "" : ", ")
				.append(f));
		fieldNames.forEach(f -> holders.append(holders.length() == 0 ? "?"
				: ",?"));
		sql.insert(0, "insert into " + tableName + "(").append(") values (")
				.append(holders).append(")");
		return getConn().prepareStatement(sql.toString());
	}

	protected PreparedStatement createUpdateStatement(String tableName,
			List<String> fieldNames, String primaryKeyField)
			throws SQLException {
		StringBuilder sql = new StringBuilder(512);
		fieldNames.forEach(f -> sql.append(sql.length() == 0 ? "" : ", ")
				.append(f).append(" = ?"));
		sql.insert(0, "update " + tableName + " set ").append(" where ")
				.append(primaryKeyField).append(" = ?");
		return getConn().prepareStatement(sql.toString());
	}

	protected Map<String, Object> findRecord(PreparedStatement selectStm,
			List<String> fieldNames, Object primaryKey) throws SQLException {
		selectStm.clearParameters();
		selectStm.setObject(1, primaryKey);
		Map<String, Object> result = null;
		try (ResultSet rst = selectStm.executeQuery()) {
			if (rst.next()) {
				result = new HashMap<>();
				for (String fieldName : fieldNames) {
					result.put(fieldName, rst.getObject(fieldName));
				}
			}
		}
		return result;
	}

	// TODO: Null values and/or missing columns?

	protected int insertRecord(PreparedStatement insertStm,
			List<String> fieldNames, Map<String, Object> record)
			throws SQLException {
		insertStm.clearParameters();
		int paramNo = 1;
		for (String fieldName : fieldNames) {
			insertStm.setObject(paramNo++, record.get(fieldName));
		}
		return insertStm.executeUpdate();
	}

	protected int updateRecord(PreparedStatement updateStm,
			List<String> fieldNames, Map<String, Object> record,
			Object primaryKey) throws SQLException {
		updateStm.clearParameters();
		int paramNo = 1;
		for (String fieldName : fieldNames) {
			updateStm.setObject(paramNo++, record.get(fieldName));
		}
		updateStm.setObject(paramNo, primaryKey);
		return updateStm.executeUpdate();
	}

	protected long getModifyDate(Map<String, Object> record) {
		Object value = record.get("modify_date");
		if (value != null && value instanceof Number) {
			return ((Number) value).longValue();
		}
		return 0;
	}

	protected void receiveModifiedRecords(String tableName, ZipFile zipIn,
			ZipOutputStream zipOut) throws IOException, SQLException {
		checkHasModifyDate(tableName);
		String primaryKeyField = getPrimaryKeyField(tableName);
		List<String> fieldNames = getFieldNames(tableName);
		boolean isCentral = zipOut != null;
		ZipEntry entry = zipIn.getEntry(tableName + ".json");
		if (entry == null) {
			logger.info("No file received for {}", tableName);
			return;
		}
		if (entry.getSize() == 0) {
			logger.warn("File for {} is empty.", tableName);
			return;
		}
		List<Map<String, Object>> inRecords;
		try (InputStream in = zipIn.getInputStream(entry)) {
			inRecords = loadRecords(in);
		}
		List<Map<String, Object>> outRecords = new ArrayList<>();
		try (PreparedStatement selectStm = createSeletStatement(tableName,
				fieldNames, primaryKeyField);
				PreparedStatement insertStm = createInsertStatement(tableName,
						fieldNames);
				PreparedStatement updateStm = createUpdateStatement(tableName,
						fieldNames, primaryKeyField);) {
			for (Map<String, Object> inRecord : inRecords) {
				Object primaryKey = inRecord.get(primaryKeyField);
				Map<String, Object> dbRecord = findRecord(selectStm,
						fieldNames, primaryKey);
				if (dbRecord == null) {
					insertRecord(insertStm, fieldNames, inRecord);
					if (isCentral) {
						outRecords.add(inRecord);
					}
				} else {
					long inModifyDate = getModifyDate(inRecord);
					long dbModifyDate = getModifyDate(dbRecord);
					if (inModifyDate > dbModifyDate) {
						updateRecord(updateStm, fieldNames, inRecord,
								primaryKey);
						if (isCentral) {
							outRecords.add(inRecord);
						}
					} else if (inModifyDate < dbModifyDate && isCentral) {
						outRecords.add(dbRecord);
					}
				}
			}
		}
		if (isCentral && !outRecords.isEmpty()) {
			addRecordsFile(tableName, outRecords, zipOut);
		}
	}

	protected void receiveModifiedData(ZipFile zipIn, ZipOutputStream zipOut)
			throws Exception {
		getConn().setAutoCommit(false);
		try {
			lockTables(getInboundTables());
			try {
				for (String tableName : getInboundTables()) {
					receiveModifiedRecords(tableName, zipIn, zipOut);
				}
			} finally {
				unlockTables();
			}
			getConn().commit();
		} catch (Exception ex) {
			getConn().rollback();
			throw ex;
		}
	}

	public String centralReceiveModifiedData(String zipInFileName,
			String outputDir) throws Exception {
		logger.info("Central receive modified data - start");
		String result = MessageFormat.format(
				"{0}{1,date,yyyyMMdd_HHmmss}_{2,number,0}.zip", outputDir
						+ (outputDir.endsWith("/") ? "" : "/"), new Date(),
				getServerId());
		logger.info("Generated zip file name: {}", result);
		try (ZipFile zipIn = new ZipFile(zipInFileName);
				OutputStream out = new FileOutputStream(result);
				ZipOutputStream zipOut = new ZipOutputStream(out)) {
			receiveModifiedData(zipIn, zipOut);
		}
		logger.info("Central receive modified data - end");
		return result;
	}

	public void remoteReceiveModifiedData(String zipInFileName)
			throws Exception {
		logger.info("Remote receive modified data - start");
		try (ZipFile zipIn = new ZipFile(zipInFileName)) {
			receiveModifiedData(zipIn, null);
		}
		logger.info("Remote receive modified data - end");
	}

}
