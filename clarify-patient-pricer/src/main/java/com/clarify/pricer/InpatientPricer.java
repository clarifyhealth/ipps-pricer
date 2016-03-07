package com.clarify.pricer;

import java.math.BigDecimal;
import java.math.RoundingMode;
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

// BIG TODO:
// Should we use BigDecimal for all the operations?
// 

public abstract class InpatientPricer {

	// 88 PAY-PERDIEM-DAYS VALUE 03.
	public static int PAY_PERDIEM_DAYS = 3;
	// 88 PAY-XFER-NO-COST VALUE 06.
	public static int PAY_XFER_NO_COST = 6;
	// 88 PAY-XFER-SPEC-DRG VALUE 09.
	public static int PAY_XFER_SPEC_DRG = 9;

	// TODO: not quite sure if this are used when computing
	// because INVDRV convert from flags to procedure/diagnostic codes.
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

	private String providerCode;
	private String drgCode;
	private LocalDate admitDate;
	private LocalDate dischargeDate;
	private double billCharges;
	private boolean costOutlierThreshold;
	private boolean hmoPaidClaim;
	private boolean transfer;
	private boolean postAcuteTransfer;
	private int reviewCode;
	private Map<String, Boolean> flags;
	private Map<String, Number> otherValues;

	private DrgDsc drgDsc;
	private DrgTab drgTab;
	private Prov prov;
	private Cbsa wageIndexRec;
	private PPHoldArea holdArea = new PPHoldArea();
	private PPSData ppsdata = new PPSData();

	protected String getProviderCode() {
		return providerCode;
	}

	protected String getDrgCode() {
		return drgCode;
	}

	protected LocalDate getAdmitDate() {
		return admitDate;
	}

	protected LocalDate getDischargeDate() {
		return dischargeDate;
	}

	protected int getDaysBetweenAdmitDischage() {
		return admitDate.until(dischargeDate).getDays();
	}

	/**
	 * <p>
	 * INDRV:
	 * </p>
	 * <ul>
	 * <li>WK-LOS</li>
	 * <li>B-LOS</li>
	 * <li>B-COVERED-DAYS</li>
	 * </ul>
	 * 
	 * @return
	 */
	protected int getLeghtOfStay() {
		int len = getDaysBetweenAdmitDischage();
		if (len == 0) {
			len = 1;
		}
		return len;
	}

	protected double getBillCharges() {
		return billCharges;
	}

	protected boolean isCostOutlierThreshold() {
		return costOutlierThreshold;
	}

	protected boolean isHmoPaidClaim() {
		return hmoPaidClaim;
	}

	protected boolean isTransfer() {
		return transfer;
	}

	protected boolean isPostAcuteTransfer() {
		return postAcuteTransfer;
	}

	protected int getReviewCode() {
		return reviewCode;
	}

	protected void setReviewCode(int reviewCode) {
		this.reviewCode = reviewCode;
	}

	protected Map<String, Boolean> getFlags() {
		return flags;
	}

	protected Map<String, Number> getOtherValues() {
		return otherValues;
	}

	protected DrgDsc getDrgDsc() {
		return drgDsc;
	}

	protected DrgTab getDrgTab() {
		return drgTab;
	}

	protected Prov getProv() {
		return prov;
	}

	protected Cbsa getWageIndexRec() {
		return wageIndexRec;
	}

	protected PPHoldArea getHoldArea() {
		return holdArea;
	}

	protected PPSData getPpsdata() {
		return ppsdata;
	}

	protected List<ProcDiag> procDiagList;

	protected abstract LocalDate getMinDischargeDate();

	protected abstract LocalDate getMaxDischargeDate();

	protected abstract String[] getFlagNames();

	protected abstract String[] getConflictingFlags();

	protected abstract String[] getOtherValueNames();

	private Connection connection;

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

	protected void validateTransfer() {
		if (transfer && postAcuteTransfer) {
			throw new IllegalArgumentException("Cannot have both transfers.");
		}
	}

	protected void validate() {
		validateRequired();
		validateDrgCode();
		validateDates();
		validateFlags();
		validateConflictingFlags();
		validateTransfer();
	}

	protected abstract void fillProcDiagList();

	protected void addToProcDiagList(String procCode, String diagCode) {
		procDiagList.add(new ProcDiag(procCode, diagCode));
	}

	protected void addToProcDiagList(String procCode) {
		procDiagList.add(new ProcDiag(procCode));
	}

	protected Cbsa findWageIndexRec() throws SQLException {
		String specPayInd = getProv().getP_cbsa_spec_pay_ind();
		String cbsaLoc = getProv().getP_cbsa_geo_loc();
		if ("Y".equals(specPayInd)) {
			cbsaLoc = getProv().getP_cbsa_reclass_loc();
		}
		if (getProv().isIndianHealtService()) {
			if (getProv().isAlaska()) {
				cbsaLoc = "   98";
			} else {
				cbsaLoc = "   99";
			}
		}
		// INVDRV.0200-GET-CBSAPR
		if (getProv().isPuertoRico()) {
			if ("Y2".contains(specPayInd)) {
				cbsaLoc = getProv().getP_cbsa_reclass_loc();
			} else {
				cbsaLoc = getProv().getP_cbsa_geo_loc();
			}
			cbsaLoc = cbsaLoc.substring(0, 4) + "*";
		}
		Cbsa result = findCbsa(cbsaLoc, dischargeDate);
		if (result != null) {
			if ("12".contains(specPayInd)) {
				result.setF_cbsa_wage_indx1(getProv().getP_cbsa_spec_wi());
			}
			if ("Y".equals(specPayInd)) {
				result.setF_cbsa_wage_indx1(result.getF_cbsa_wage_indx2());
			}
			String size = result.getF_cbsa_size();
			if (getProv().isCbsaStdRuralCheck()) {
				size = Cbsa.SIZE_ALL_RURAL;
			} else if (!Cbsa.SIZE_LARGE_URBAN.equals(size)) {
				size = Cbsa.SIZE_OTHER_URBAN;
			}
			result.setF_cbsa_size(size);
		}
		return result;
	}

	protected BigDecimal double2bd(double value, int scale) {
		return BigDecimal.valueOf(value)
				.setScale(scale, RoundingMode.HALF_EVEN);
	}

	protected double bd2double(BigDecimal value) {
		return value == null ? 0 : value.doubleValue();
	}

	/**
	 * <p>
	 * PPCAL.1000-EDIT-THE-BILL-INFO
	 * </p>
	 */
	protected void editTheBillInfo() {
		holdArea.setCapiPaycdePct1(BigDecimal.ONE);
		holdArea.setCapiPaycdePct2(BigDecimal.ZERO);
		holdArea.setCovDays(getLeghtOfStay());
		holdArea.setRegDays(getLeghtOfStay());
	}

	/**
	 * <p>
	 * PPCAL.2000-ASSEMBLE-PPS-VARIABLES
	 * </p>
	 */
	protected void assemblePPSVariables() {
		holdArea.setFacSpecRate(getProv().getP_capi_hosp_spec_rate());
		holdArea.setInternRatio(getProv().getP_intern_ratio());
		if (getProv().isAlaska() || getProv().isHawaii()) {
			holdArea.setOperCola(getProv().getP_cola());
		} else {
			holdArea.setOperCola(BigDecimal.ONE);
		}
		getDrgWeigth();
		holdArea.setWageIndex(getWageIndexRec().getF_cbsa_wage_indx1());
		holdArea.setPrWageIndex(getWageIndexRec().getF_cbsa_wage_indx1());
		getLaborRates();
		holdArea.setOperHspPct(BigDecimal.ZERO);
		holdArea.setOperFspPct(BigDecimal.ONE);
		holdArea.setNatPct(BigDecimal.ONE);
		holdArea.setRegPct(BigDecimal.ZERO);
		if (getProv().isPuertoRico()) {
			holdArea.setNatPct(0.75);
			holdArea.setRegPct(0.25);
		}
		if (getProv().isSchRebasedFY90() || getProv().isEach()
				|| getProv().isMdhRebasedFY90()) {
			holdArea.setOperHspPct(BigDecimal.ONE);
		}
	}

	/**
	 * <p>
	 * PPCAL.2600-GET-DRG-WEIGHT
	 * </p>
	 */
	protected void getDrgWeigth() {
		holdArea.setDrgWt(drgTab.getDrg_weight());
		holdArea.setAlos(drgTab.getDrg_gmalos());
		holdArea.setArithAlos(drgTab.getDrg_arith_alos());
		holdArea.setDaysCutoff(BigDecimal.ZERO);
	}

	protected void getLaborRates() {
		// TODO: implement
	}

	/**
	 * <p>
	 * PPCAL.3000-CALC-PAYMENT
	 * </p
	 */
	protected void calcPayment() {
		calcStayUtilization();
		calcOperFspAmt();
		calOperDsh();

		double interRatio = bd2double(holdArea.getInternRatio());
		double operImeTeach = 1.35 * (Math.pow(1 + interRatio, .405) + 1);
		holdArea.setOperImeTeach(double2bd(operImeTeach, 9));
		ppsdata.setWageIndx(holdArea.getWageIndex());
		ppsdata.setAvgLos(holdArea.getAlos());
		ppsdata.setDaysCutoff(holdArea.getDaysCutoff());
		holdArea.setPerdiemDays(getLeghtOfStay() + 1);
		holdArea.setDschgFrctn(BigDecimal.ONE);
		holdArea.setDrgWtFrctn(holdArea.getDrgWt());

		if ((reviewCode == PAY_PERDIEM_DAYS || reviewCode == PAY_XFER_NO_COST)
				|| (reviewCode == PAY_XFER_SPEC_DRG && drgTab
						.isDrgPostacutePerdiem())) {
			if (holdArea.getAlos().doubleValue() > 0) {
				if (holdArea.getDschgFrctn().doubleValue() > 1) {
					double computed = (double) holdArea.getPerdiemDays() / bd2double(holdArea.getAlos());
					holdArea.setTransferAdj(double2bd(computed, 4));
					holdArea.setDschgFrctn(double2bd(computed, 4));
					if (computed > 1) {
						holdArea.setTransferAdj(BigDecimal.ONE);
						holdArea.setDschgFrctn(BigDecimal.ONE);
					} else {
						// double drgWt = bd2double(holdArea.getDrgWt());
						holdArea.setDrgWt(double2bd(holdArea.getTransferAdj()
								.doubleValue()
								* holdArea.getDrgWt().doubleValue(), 4));
					}
				}
			}
		}
	}

	/**
	 * <p>
	 * PPCAL.3100-CALC-STAY-UTILIZATION
	 * </p>
	 */
	protected void calcStayUtilization() {
		// 260900 3100-CALC-STAY-UTILIZATION.
		// 261000
		// 261100 MOVE 0 TO PPS-REG-DAYS-USED.
		// 261200 MOVE 0 TO PPS-LTR-DAYS-USED.
		// 261300
		// 261400 IF H-REG-DAYS > 0
		// 261500 IF H-REG-DAYS > B-LOS
		// 261600 MOVE B-LOS TO PPS-REG-DAYS-USED
		// 261700 ELSE
		// 261800 MOVE H-REG-DAYS TO PPS-REG-DAYS-USED
		// 261900 ELSE
		// 262000 IF H-LTR-DAYS > B-LOS
		// 262100 MOVE B-LOS TO PPS-LTR-DAYS-USED
		// 262200 ELSE
		// 262300 MOVE H-LTR-DAYS TO PPS-LTR-DAYS-USED.
		// TODO: implement

	}

	/**
	 * <p>
	 * PPCAL.3300-CALC-OPER-FSP-AMT
	 * </p>
	 */
	protected void calcOperFspAmt() {
		// TODO: implement
	}

	/**
	 * <p>
	 * 3900A-CALC-OPER-DSH - 3900A-EXIT
	 * </p>
	 */
	protected void calOperDsh() {
		// TODO: implement
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
		this.prov = findProv(getProviderCode(), dischargeDate);
		this.procDiagList = new ArrayList<>();

		// Basic validations
		validate();

		// Business validations
		if (this.prov == null) {
			return PricerResult.build(PricerResult.RST_NO_PROVIDER_INFO);
		}
		boolean newHosp = "Y".equals(getProv().getP_capi_new_hosp());
		String payCode = getProv().getP_capi_pps_pay_code();
		if (!newHosp && "BC".indexOf(payCode) == -1) {
			return PricerResult.build(PricerResult.RST_PAY_CODE_NOT_ABC);
		}
		String geoLoc = this.getProv().getP_cbsa_geo_loc();
		if (geoLoc != null && geoLoc.indexOf('*') != -1) {
			return PricerResult.build(PricerResult.RST_INVALID_CBSA_OR_WI);
		}
		this.wageIndexRec = findWageIndexRec();
		if (wageIndexRec == null) {
			return PricerResult.build(PricerResult.RST_INVALID_CBSA_OR_WI);
		}

		// Assign review code
		if (transfer) {
			reviewCode = PAY_PERDIEM_DAYS;
		}
		if (postAcuteTransfer && drgTab.isDrgPostacute5050()) {
			reviewCode = PAY_XFER_SPEC_DRG;
		}

		// TODO: should do this?
		fillProcDiagList();

		editTheBillInfo();
		assemblePPSVariables();
		calcPayment();

		return null;
	}
}
