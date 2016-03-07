package com.clarify;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ToLayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;

public class CobolUtils {
	public enum TableAction {
		NONE, TRUNCATE, RECREATE
	}

	private CopybookLoader copyBookLoader = new CobolCopybookLoader();

	protected CopybookLoader getCopybookLoader() {
		if (copyBookLoader == null) {
			copyBookLoader = new CobolCopybookLoader();
		}
		return copyBookLoader;
	}

	public List<IFieldDetail> getFieldDetailList(LayoutDetail copyBook) {
		return copyBook
				.getFieldNameMap()
				.entrySet()
				.stream()
				.map(Map.Entry::getValue)
				.sorted((fd1, fd2) -> Integer.compare(fd1.getPos(),
						fd2.getPos())).collect(Collectors.toList());
	}

	public String toMySQLFieldDef(IFieldDetail fieldDetail, boolean lowerCase) {
		// Type
		// 0 = PIC X
		// 25 = PIC 9...
		// 22 = PIC 9...V9..
		// 41 = PIC S9...V99
		String name = fieldDetail.getName();
		int len = fieldDetail.getLen();
		int decimal = fieldDetail.getDecimal();
		String type;
		if (decimal == 0) {
			if (fieldDetail.getType() == 0) {
				if (name.endsWith("-DATE") && len == 8) {
					type = "date";
				} else {
					type = len == 1 ? "char(%d)" : "varchar(%d)";
				}
			} else {
				if (len < 9) {
					type = "int";
				} else if (len < 19) {
					type = "bigint";
				} else {
					type = "numeric(%d)";
				}
			}
		} else {
			type = "numeric(%d, %d)";
		}
		name = name.replace('-', '_');
		if (lowerCase) {
			name = name.toLowerCase();
		}
		return String.format("%s " + type, name, len, decimal);
	}

	private static final SimpleDateFormat SIMPLE_DATE_FORMAT = new SimpleDateFormat(
			"yyyyMMdd");

	public Object getFieldValue(AbstractLine line, IFieldDetail fieldDetail)
			throws ParseException {
		String fieldName = fieldDetail.getName();
		AbstractFieldValue fieldValue = line.getFieldValue(fieldName);
		String value = fieldValue.asString();
		int len = fieldDetail.getLen();
		int decimal = fieldDetail.getDecimal();
		Object result = null;
		if (decimal == 0) {
			if (fieldDetail.getType() == 0) {
				if (fieldName.endsWith("-DATE") && len == 8) {
					if (value != null && !"".equals(value)
							&& !value.matches("0+")) {
						result = SIMPLE_DATE_FORMAT.parse(value);
					}
				} else {
					result = value;
				}
			} else {
				if (len < 9) {
					result = fieldValue.asInt();
				} else if (len < 19) {
					result = fieldValue.asLong();
				} else {
					result = fieldValue.asBigInteger();
				}
			}
		} else {
			result = fieldValue.asBigDecimal();
		}
		return result;
	}

	public Object getFieldValue(AbstractLine line, String fieldName)
			throws ParseException {
		return getFieldValue(line, line.getFieldValue(fieldName)
				.getFieldDetail());
	}

	public Object[] getFieldValues(AbstractLine line,
			List<IFieldDetail> fieldDetailList) {
		List<Object> result = new ArrayList<>();
		fieldDetailList.stream().forEach(fd -> {
			try {
				result.add(getFieldValue(line, fd));
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		});
		return result.toArray();
	}

	public String toMySQLCreateTable(List<IFieldDetail> fieldDetailList,
			String tableName, boolean ifNotExists, boolean lowerCase,
			String... primaryKey) {
		StringBuilder sb = new StringBuilder();
		fieldDetailList.stream().forEach(fd -> {
			if (sb.length() > 0) {
				sb.append("\n\t,");
			} else {
				sb.append("\t");
			}
			sb.append(toMySQLFieldDef(fd, lowerCase));
		});
		sb.insert(0, "create table " + (ifNotExists ? " if not exists " : "")
				+ (lowerCase ? tableName.toLowerCase() : tableName) + " (\n");
		if (primaryKey != null) {
			boolean first = true;
			for (String key : primaryKey) {
				if (first) {
					sb.append("\n\t,primary key (");
					first = false;
				} else {
					sb.append(", ");
				}
				key = key.replace('-', '_');
				if (lowerCase) {
					key = key.toLowerCase();
				}
				sb.append(key);
			}
			if (!first) {
				sb.append(')');
			}
		}
		sb.append("\n)");
		return sb.toString();
	}

	public String toMySQLCreateTable(LayoutDetail copyBook, String tableName,
			boolean ifNotExists, boolean lowerCase, String... primaryKey) {
		return toMySQLCreateTable(getFieldDetailList(copyBook), tableName,
				ifNotExists, lowerCase, primaryKey);
	}

	public String toMySQLCreateTable(LayoutDetail copyBook, String tableName,
			boolean lowerCase, String... primaryKey) {
		return toMySQLCreateTable(getFieldDetailList(copyBook), tableName,
				true, lowerCase, primaryKey);
	}

	public String toMySQLCreateTable(String copyBookFileName, int binFormat,
			String tableName, boolean ifNotExist, boolean lowerCase,
			String... primaryKey) throws Exception {
		LayoutDetail copyBook = ToLayoutDetail.getInstance().getLayout(
				getCopybookLoader().loadCopyBook(copyBookFileName,
						CopybookLoader.SPLIT_NONE, 0, "",
						CommonBits.getDefaultCobolTextFormat(), binFormat, 0,
						null));
		return toMySQLCreateTable(getFieldDetailList(copyBook), tableName,
				ifNotExist, lowerCase, primaryKey);
	}

	public String toMySQLCreateTable(String copyBookFileName, int binFormat,
			String tableName, boolean lowerCase, String... primaryKey)
			throws Exception {
		return toMySQLCreateTable(copyBookFileName, binFormat, tableName, true,
				lowerCase, primaryKey);
	}

	public String toMySQLInsert(List<IFieldDetail> fieldDetailList,
			String tableName, boolean lowerCase) {
		StringBuilder sb = new StringBuilder();
		fieldDetailList.stream().forEach(fd -> {
			if (sb.length() > 0) {
				sb.append("\n\t,");
			} else {
				sb.append("\t");
			}
			String fieldName = fd.getName().replace('-', '_');
			if (lowerCase) {
				fieldName = fieldName.toLowerCase();
			}
			sb.append(fieldName);
		});
		sb.insert(
				0,
				"insert into "
						+ (lowerCase ? tableName.toLowerCase() : tableName)
						+ " (\n").append(")\nvalues (");
		for (int i = 0; i < fieldDetailList.size(); i++) {
			if (i == 0) {
				sb.append("?");
			} else {
				sb.append(", ?");
			}
		}
		sb.append(")");
		return sb.toString();
	}

	public String toMySQLInsert(LayoutDetail copyBook, String tableName,
			boolean lowerCase) {
		return toMySQLInsert(getFieldDetailList(copyBook), tableName, lowerCase);
	}

	public void convertToMySQL(Connection con, String tableName,
			boolean lowerCase, AbstractLineReader reader) throws SQLException,
			IOException {
		boolean saveAutoCommit = con.getAutoCommit();
		con.setAutoCommit(false);
		try {
			String sql = toMySQLInsert(reader.getLayout(), tableName, lowerCase);
			PreparedStatement stm = con.prepareStatement(sql);
			List<IFieldDetail> fieldDetailList = getFieldDetailList(reader
					.getLayout());
			AbstractLine line;
			while ((line = reader.read()) != null) {
				Object[] record = getFieldValues(line, fieldDetailList);
				stm.clearParameters();
				int index = 1;
				for (Object value : record) {
					stm.setObject(index++, value);
				}
				stm.execute();
			}
			con.commit();
		} catch (Exception ex) {
			con.rollback();
			if (ex instanceof SQLException) {
				throw (SQLException) ex;
			} else {
				throw new RuntimeException(ex);
			}
		} finally {
			con.setAutoCommit(saveAutoCommit);
		}
	}

	public void convertToMySQL(Connection con, String tableName,
			boolean lowerCase, TableAction tableAction, String[] primaryKey,
			String copyBookFileName, String dataFileName, int fileFormat,
			int binFormat) throws Exception {
		LayoutDetail copyBook = ToLayoutDetail.getInstance().getLayout(
				getCopybookLoader().loadCopyBook(copyBookFileName,
						CopybookLoader.SPLIT_NONE, 0, "",
						CommonBits.getDefaultCobolTextFormat(), binFormat, 0,
						null));
		@SuppressWarnings("deprecation")
		AbstractLineReader reader = LineIOProvider.getInstance().getLineReader(
				fileFormat);
		reader.open(dataFileName, copyBook);
		try {
			Statement stm = con.createStatement();
			switch (tableAction) {
			case NONE:
				break;
			case TRUNCATE:
				try {
					stm.execute(String.format("truncate table %s",
							(lowerCase ? tableName.toLowerCase() : tableName)));
				} catch (SQLException e) {
					// Ignore
				}
				break;
			case RECREATE:
				try {
					stm.execute(String.format("drop table %s",
							(lowerCase ? tableName.toLowerCase() : tableName)));
				} catch (SQLException e) {
					// Ignore
				}
				String sql = toMySQLCreateTable(copyBook, tableName, lowerCase,
						primaryKey);
				stm.execute(sql);
				break;
			}
			convertToMySQL(con, tableName, lowerCase, reader);
		} finally {
			reader.close();
		}
	}

	public void convertToMySQL(String url, String user, String password,
			String tableName, TableAction tableAction, String[] primaryKey,
			String copyBookFileName, String dataFileName, int fileFormat,
			int binFormat) throws Exception {
		try (Connection con = DriverManager.getConnection(url, user, password)) {
			convertToMySQL(con, tableName, false, tableAction, primaryKey,
					copyBookFileName, dataFileName, fileFormat, binFormat);
		}
	}

	public List<String> extractTextToList(InputStream in, String startExp,
			String endExp, String dataExp, int group) throws IOException {
		List<String> result = new ArrayList<>();
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));
		Pattern dataPattern = Pattern.compile(dataExp);
		boolean started = false;
		String line;
		while ((line = reader.readLine()) != null) {
			if (!started && line.matches(startExp)) {
				started = true;
				continue;
			}
			if (!started) {
				continue;
			}
			if (line.matches(endExp)) {
				break;
			}
			Matcher matcher = dataPattern.matcher(line);
			if (matcher.find()) {
				if (group > -1) {
					result.add(matcher.group(group));
				} else {
					result.add(line);
				}
			}
		}
		return result;
	}

	public List<String> extractTextToList(String sourceFileName,
			String startExp, String endExp, String dataExp, int group)
			throws IOException {
		try (InputStream in = new FileInputStream(sourceFileName)) {
			return extractTextToList(in, startExp, endExp, dataExp, group);
		}
	}

	public void extractText(InputStream in, OutputStream out, String startExp,
			String endExp, String dataExp, int group) throws IOException {
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));
		PrintWriter writer = new PrintWriter(out);
		Pattern dataPattern = Pattern.compile(dataExp);
		boolean started = false;
		String line;
		while ((line = reader.readLine()) != null) {
			if (!started && line.matches(startExp)) {
				started = true;
				continue;
			}
			if (!started) {
				continue;
			}
			if (line.matches(endExp)) {
				break;
			}
			Matcher matcher = dataPattern.matcher(line);
			if (matcher.find()) {
				if (group > -1) {
					writer.println(matcher.group(group));
				} else {
					writer.println(line);
				}
			}
		}
		writer.flush();
	}

	public void extractText(String sourceFileName, String outFileName,
			String startExp, String endExp, String dataExp, int group)
			throws FileNotFoundException, IOException {
		try (InputStream in = new FileInputStream(sourceFileName);
				OutputStream out = new FileOutputStream(outFileName)) {
			extractText(in, out, startExp, endExp, dataExp, group);
		}
	}

	public void removeLineNumbers(InputStream in, OutputStream out)
			throws IOException {
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(
				in)); OutputStreamWriter writer = new OutputStreamWriter(out);) {
			String line;
			while ((line = reader.readLine()) != null) {
				if (line.length() > 6) {
					line = "      " + line.substring(6).replaceAll("\\s+$", "");
				} else
					line = "";
				writer.write(line);
				writer.write('\n');
			}
			writer.flush();
		}
	}

	public void removeLineNumbers(String sourceFileName, String destFileName)
			throws FileNotFoundException, IOException {
		try (InputStream in = new FileInputStream(sourceFileName);
				OutputStream out = new FileOutputStream(destFileName)) {
			removeLineNumbers(in, out);
		}
	}

	public static String toCamelCase(String cobolIdentifier, boolean firstlower) {
		String result = "";
		String[] parts = cobolIdentifier.split("-");
		boolean first = true;
		for (String part : parts) {
			if (first && firstlower) {
				part = part.toLowerCase();
			} else {
				part = part.substring(0, 1).toUpperCase()
						+ (part.length() > 1 ? part.substring(1).toLowerCase()
								: "");
			}
			result += part;
			first = false;
		}
		return result;
	}

	public static String toCamelCase(String cobolIdentifier) {
		return toCamelCase(cobolIdentifier, true);
	}
}
