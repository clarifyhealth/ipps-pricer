package com.clarify.pricer;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.dbutils.QueryRunner;
import org.apache.commons.dbutils.handlers.BeanHandler;

import com.clarify.pricer.dto.Cbsa;
import com.clarify.pricer.dto.DrgDsc;
import com.clarify.pricer.dto.DrgTab;
import com.clarify.pricer.dto.Prov;

public abstract class InpatientPricer {

	// TODO: check logic...
	// public static final int RST_CODE_FULL_DRG_PAYMENT = 0;
	// public static final int RST_CODE_DAY_OUTLIER_PAYMENT = 2;

	public static final int RST_CODE_NO_PROVIDER_INFO = 51;
	public static final int RST_CODE_INVALID_CBSA_OR_WI = 52;

	public static final Map<Integer, String> resultCodes = new HashMap<>();

	static {
		// TODO: fill codes
		resultCodes.put(RST_CODE_NO_PROVIDER_INFO,
				"NO PROVIDER SPECIFIC INFO FOUND OR PROVIDER TERMINATED");
		resultCodes.put(RST_CODE_INVALID_CBSA_OR_WI,
				"INVALID CBSA # IN PSF OR INVALID WAGE INDEX");
	}

	public static class PricerResult {
		private int code;
		private String reason;
		private Map<String, Number> values = new HashMap<>();

		public PricerResult() {
		}

		public PricerResult(int code) {
			setCode(code);
		}

		public PricerResult(int code, Map<String, Number> values) {
			this(code);
			this.values = values;
		}

		public int getCode() {
			return code;
		}

		public void setCode(int code) {
			this.code = code;
			this.reason = resultCodes.get(code);
		}

		public String getReason() {
			return reason;
		}

		public void setReason(String reason) {
			this.reason = reason;
		}

		public Map<String, Number> getValues() {
			return values;
		}

		public static PricerResult build(int code) {
			return new PricerResult(code);
		}
	}

	public static class ProcDiag {
		private String procCode;
		private String diagCode;

		public ProcDiag(String procCode) {
			this.procCode = procCode;
		}

		public ProcDiag(String procCode, String diagCode) {
			this.procCode = procCode;
			this.diagCode = diagCode;
		}

		public String getProcCode() {
			return procCode;
		}

		public String getDiagCode() {
			return diagCode;
		}
	}

	private Connection connection;

	protected String providerCode;
	protected String drgCode;
	protected LocalDate admitDate;
	protected LocalDate dischargeDate;
	protected double billCharges;
	protected boolean costOutlierThreshold;
	protected boolean hmoPaidClaim;
	protected boolean transfer;
	protected boolean postAcuteTransfer;
	protected Map<String, Boolean> flags;
	protected Map<String, Number> otherValues;
	protected DrgDsc drgDsc;
	protected DrgTab drgTab;
	protected Prov prov;
	protected PricerResult pricerResult;
	protected List<ProcDiag> procDiagList;
	protected Map<String, Number> computedValues;

	protected Cbsa wageIndex;

	protected abstract LocalDate getMinDischargeDate();

	protected abstract LocalDate getMaxDischargeDate();

	protected abstract String[] getFlagNames();

	protected abstract String[] getConflictingFlags();

	protected abstract String[] getOtherValueNames();

	protected Connection getConnection() {
		return connection;
	}

	protected void setConnection(Connection connection) {
		this.connection = connection;
	}

	protected static final String SQL_FIND_DRG_DSC = ""
			+ "select * from drgdsc where drg_code = ?";

	protected DrgDsc findDrgDsc(String drgCode) throws SQLException {
		QueryRunner runner = new QueryRunner();
		BeanHandler<DrgDsc> handler = new BeanHandler<DrgDsc>(DrgDsc.class);
		return runner
				.query(getConnection(), SQL_FIND_DRG_DSC, handler, drgCode);

	}

	protected static final String SQL_FIND_DRG_TAB = ""
			+ "select * from drg_tab where drg_code = ?";

	protected DrgTab findDrgTab(String drgCode) throws SQLException {
		QueryRunner runner = new QueryRunner();
		BeanHandler<DrgTab> handler = new BeanHandler<DrgTab>(DrgTab.class);
		return runner
				.query(getConnection(), SQL_FIND_DRG_TAB, handler, drgCode);
	}

	protected static final String SQL_FIND_PROV = "" //
			+ "select * from prov "
			+ "where p_provider_no = ? and p_effect_date >= ? "
			+ "and (p_termination_date is null or p_termination_date < ?) "
			+ "order by p_provider_no, p_effect_date limit 1";

	protected Prov findProv(String provCode, LocalDate dischargeDate)
			throws SQLException {
		QueryRunner runner = new QueryRunner();
		BeanHandler<Prov> handler = new BeanHandler<Prov>(Prov.class);
		java.sql.Date dd = java.sql.Date.valueOf(dischargeDate);
		return runner.query(getConnection(), SQL_FIND_PROV, handler, provCode,
				dd, dd);
	}

	protected static final String SQL_FIND_CBSA = ""
			+ "select * from cbsa "
			+ "where f_cbsa = ? and f_cbsa_eff_date >= ? order by f_cbsa, f_cbsa_eff_date limit 1";

	protected Cbsa findCbsa(String cbsaLoc, LocalDate dischargeDate)
			throws SQLException {
		QueryRunner runner = new QueryRunner();
		BeanHandler<Cbsa> handler = new BeanHandler<Cbsa>(Cbsa.class);
		return runner.query(getConnection(), SQL_FIND_CBSA, handler, cbsaLoc,
				java.sql.Date.valueOf(dischargeDate));
	}

	protected void validateDrgCode() {
		if ("998".equals(drgCode) || "999".equals(drgCode)) {
			throw new IllegalArgumentException(String.format(
					"Invalid DRG code \"%s\"", drgCode));
		}
		if (drgDsc == null) {
			throw new IllegalArgumentException(String.format(
					"DRG code \"%s\" not found.", drgCode));
		}
		if (drgTab == null) {
			throw new IllegalArgumentException(String.format(
					"DRG weight for \"%s\" not found.", drgCode));
		}
	}

	protected void validateRequired() {
		Objects.requireNonNull(providerCode, "Provider code required.");
		Objects.requireNonNull(drgCode, "DRG code required.");
		Objects.requireNonNull(admitDate, "Admit date required.");
		Objects.requireNonNull(dischargeDate, "Discharge date required.");
		if (billCharges < 0) {
			throw new IllegalArgumentException(String.format(
					"Invalid bill charges: %d", billCharges));
		}
	}

	protected void validateDates() {
		if (dischargeDate.isBefore(admitDate)) {
			throw new IllegalArgumentException(
					String.format(
							"Discharge date (\"%s\") cannot be less than amit date (\"%s\")",
							dischargeDate.toString(), admitDate.toString()));
		}
		if (dischargeDate.isBefore(getMinDischargeDate())) {
			throw new IllegalArgumentException(String.format(
					"Discharge date must be >= \"%s\"", getMinDischargeDate()
							.toString()));
		}
		if (dischargeDate.isAfter(getMaxDischargeDate())) {
			throw new IllegalArgumentException(String.format(
					"Discharge date must be <= \"%s\"", getMaxDischargeDate()
							.toString()));
		}
		long days = ChronoUnit.DAYS.between(admitDate, dischargeDate);
		if (days > 999) {
			throw new IllegalArgumentException(String.format(
					"Invalid covered days: %d", days));
		}
	}

	protected void validateFlags() {
		if (transfer && postAcuteTransfer) {
			throw new IllegalArgumentException(
					"Both transfer flags cannot be true.");
		}
		List<String> validFlagNames = (List<String>) Arrays
				.asList(getFlagNames());
		flags.keySet().forEach(
				k -> {
					if (validFlagNames.indexOf(k) == -1) {
						throw new IllegalArgumentException(String.format(
								"Invalid flag: \"%s\"", k));
					}
				});
	}

	protected List<String> getFlagsInConflict() {
		List<String> result = new ArrayList<>();
		for (String flag : getConflictingFlags()) {
			if (flags.getOrDefault(flag, false)) {
				result.add(flag);
			}
		}
		return result;
	}

	protected void validateConflictingFlags() {
		List<String> inConflict = getFlagsInConflict();
		if (inConflict.size() > 1) {
			throw new IllegalArgumentException(String.format(
					"Flags in conflict:", String.join(", ", inConflict)));
		}
	}

	protected void validate() {
		validateRequired();
		validateDrgCode();
		validateDates();
		validateFlags();
		validateConflictingFlags();
	}

	protected abstract void fillProcDiagList();

	protected void addToProcDiagList(String procCode, String diagCode) {
		procDiagList.add(new ProcDiag(procCode, diagCode));
	}

	protected void addToProcDiagList(String procCode) {
		procDiagList.add(new ProcDiag(procCode));
	}

	protected Cbsa findWageIndex() throws SQLException {
		String providerNo = prov.getP_provider_no();
		String specPayInd = prov.getP_cbsa_spec_pay_ind();
		String cbsaLoc = prov.getP_cbsa_geo_loc();
		if ("Y".equals(specPayInd)) {
			cbsaLoc = prov.getP_cbsa_reclass_loc();
		}
		if (prov.isIndianHealtService()) {
			if (providerNo.startsWith("02")) {
				cbsaLoc = "   98";
			} else {
				cbsaLoc = "   99";
			}
		}
		// Seems Puerto Rico (0200-GET-CBSAPR)
		if (providerNo.startsWith("40")) {
			if ("Y2".contains(specPayInd)) {
				cbsaLoc = prov.getP_cbsa_reclass_loc();
			} else {
				cbsaLoc = prov.getP_cbsa_geo_loc();
			}
			cbsaLoc = cbsaLoc.substring(0, 4) + "*";
		}
		Cbsa result = findCbsa(cbsaLoc, dischargeDate);
		if (result != null) {
			if ("12".contains(specPayInd)) {
				result.setF_cbsa_wage_indx1(prov.getP_cbsa_spec_wi());
			}
			if ("Y".equals(specPayInd)) {
				result.setF_cbsa_wage_indx1(result.getF_cbsa_wage_indx2());
			}
			String size = result.getF_cbsa_size();
			if (prov.isCbsaStdRuralCheck()) {
				size = Cbsa.SIZE_ALL_RURAL;
			} else if (!Cbsa.SIZE_LARGE_URBAN.equals(size)) {
				size = Cbsa.SIZE_OTHER_URBAN;
			}
			result.setF_cbsa_size(size);
		}
		return result;
	}

	public PricerResult compute( //
			String providerCode, //
			String drgCode, //
			// String patientCode,
			LocalDate admitDate, //
			LocalDate dischargeDate, //
			double billCharges, //
			boolean costOutlierThreshold, //
			boolean hmoPaidClaim, //
			boolean transfer, //
			boolean postAcuteTransfer, //
			Map<String, Boolean> flags, //
			Map<String, Number> otherValues) throws SQLException {
		this.providerCode = providerCode;
		this.drgCode = drgCode;
		this.admitDate = admitDate;
		this.dischargeDate = dischargeDate;
		this.billCharges = billCharges;
		this.costOutlierThreshold = costOutlierThreshold;
		this.hmoPaidClaim = hmoPaidClaim;
		this.transfer = transfer;
		this.postAcuteTransfer = postAcuteTransfer;
		this.flags = flags == null ? new HashMap<>() : flags;
		this.otherValues = otherValues == null ? new HashMap<>() : otherValues;
		this.drgDsc = findDrgDsc(drgCode);
		this.drgTab = findDrgTab(drgCode);
		this.prov = findProv(providerCode, dischargeDate);
		this.procDiagList = new ArrayList<>();
		this.computedValues = new HashMap<>();

		// Basic validation
		validate();

		// Business validations
		if (this.prov == null) {
			return PricerResult.build(RST_CODE_NO_PROVIDER_INFO);
		}
		String geoLoc = this.prov.getP_cbsa_geo_loc();
		if (geoLoc != null && geoLoc.indexOf('*') != -1) {
			return PricerResult.build(RST_CODE_INVALID_CBSA_OR_WI);
		}
		this.wageIndex = findWageIndex();
		if (wageIndex == null) {
			return PricerResult.build(RST_CODE_INVALID_CBSA_OR_WI);
		}

		fillProcDiagList();
		return null;
	}
}
