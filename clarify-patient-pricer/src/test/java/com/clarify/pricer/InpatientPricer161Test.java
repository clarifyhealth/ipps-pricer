package com.clarify.pricer;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDate;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class InpatientPricer161Test {

	static String url = "jdbc:mysql://localhost/inpprc161";
	static String user = "root";
	static String password = "master";
	static Connection con;

	static String DRG_CODE = "001";
	static String PROV_CODE = "010001";
	static String CBSA_LOC = "10180";

	@BeforeClass
	public static void beforeClass() throws SQLException {
		con = DriverManager.getConnection(url, user, password);
	}

	protected InpatientPricer161 createPricer() {
		InpatientPricer161 result = new InpatientPricer161();
		result.setConnection(con);
		return result;
	}

	// ----- Basic tests -----

	@Test
	public void testFindDrgDsc() throws SQLException {
		assertNotNull(createPricer().findDrgDsc(DRG_CODE));
	}

	@Test
	public void testFindDrgTab() throws SQLException {
		assertNotNull(createPricer().findDrgTab(DRG_CODE));
	}

	@Test
	public void testFindProvExists() throws SQLException {
		InpatientPricer161 pricer = createPricer();
		LocalDate dischargeDate = pricer.getMinDischargeDate();
		assertNotNull(pricer.findProv(PROV_CODE, dischargeDate));
	}

	@Test
	public void testFindProvNotExists() throws SQLException {
		InpatientPricer161 pricer = createPricer();
		LocalDate dischargeDate = pricer.getMaxDischargeDate().plusDays(1);
		assertNull(pricer.findProv(PROV_CODE, dischargeDate));
	}

	@Test
	public void testFindCbsaExists() throws SQLException {
		InpatientPricer161 pricer = createPricer();
		LocalDate dischargeDate = pricer.getMinDischargeDate();
		assertNotNull(pricer.findCbsa(CBSA_LOC, dischargeDate));
	}

	@Test
	public void testFindCbsaNotExists() throws SQLException {
		InpatientPricer161 pricer = createPricer();
		LocalDate dischargeDate = pricer.getMinDischargeDate().plusDays(1);
		assertNull(pricer.findCbsa(CBSA_LOC, dischargeDate));
	}

	@AfterClass
	public static void after() throws SQLException {
		con.close();
	}
}
