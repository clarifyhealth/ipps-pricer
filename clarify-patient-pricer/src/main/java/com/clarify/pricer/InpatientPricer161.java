package com.clarify.pricer;

import java.math.BigDecimal;
import java.time.LocalDate;

public class InpatientPricer161 extends InpatientPricer {

	// Screen items commented:
	// BUNDLE-ADJUSTMENT
	// NEWTECH-INPACT

	public static final String NEWTECH_CARDIO = "NEWTECH-CARDIO"; // 154 also
	public static final String NEWTECH_BLINAT = "NEWTECH-BLINAT";
	public static final String NEWTECH_LUTONIX = "NEWTECH-LUTONIX";
	public static final String NEWTECH_ARGUS = "NEWTECH-ARGUS";
	public static final String NEWTECH_KCENTRA = "NEWTECH-KCENTRA";
	public static final String NEWTECH_MITRCLP = "NEWTECH-MITRCLP";
	public static final String NEWTECH_RNSSYST = "NEWTECH-RNSSYST";
	public static final String NEWTECH_CELL_XFER = "NEWTECH-CELL-XFER";

	protected final static String[] FLAG_NAMES = { NEWTECH_CARDIO,
			NEWTECH_BLINAT, NEWTECH_LUTONIX, NEWTECH_ARGUS, NEWTECH_KCENTRA,
			NEWTECH_MITRCLP, NEWTECH_RNSSYST, NEWTECH_CELL_XFER };

	// "MULT TECH XFERS..." commented
	// NEWTECH-INPACT
	// NEWTECH-DIFICID
	// NEWTECH-X-STOP
	// Except:
	//

	protected final static String[] CONFLIGTING_FLAGS = { NEWTECH_CELL_XFER };

	@Override
	protected LocalDate getMinDischargeDate() {
		return LocalDate.of(2015, 10, 1);
	}

	@Override
	protected LocalDate getMaxDischargeDate() {
		return LocalDate.of(2016, 9, 30);
	}

	@Override
	protected String[] getFlagNames() {
		return FLAG_NAMES;
	}

	@Override
	protected String[] getConflictingFlags() {
		return CONFLIGTING_FLAGS;
	}

	@Override
	protected String[] getOtherValueNames() {
		return null;
	}

	// NEWTECH_ARGUS & NEWTECH_MITRCLP codes
	// are moved proc code #5, perhaps a bug in COBOL code.
	@Override
	protected void fillProcDiagList() {
		if (getFlags().getOrDefault(NEWTECH_CARDIO, false)) {
			addToProcDiagList("02HQ30Z");
		}
		if (getFlags().getOrDefault(NEWTECH_BLINAT, false)) {
			addToProcDiagList("XW03351");
		}
		if (getFlags().getOrDefault(NEWTECH_LUTONIX, false)) {
			addToProcDiagList("047K041");
		}
		if (getFlags().getOrDefault(NEWTECH_ARGUS, false)) {
			addToProcDiagList("08H005Z");
		}
		if (getFlags().getOrDefault(NEWTECH_KCENTRA, false)) {
			addToProcDiagList("30283B1");
		}
		if (getFlags().getOrDefault(NEWTECH_MITRCLP, false)) {
			addToProcDiagList("02UG3JZ");
		}
		if (getFlags().getOrDefault(NEWTECH_RNSSYST, false)) {
			addToProcDiagList("0NH00NZ");
		}
		if (getFlags().getOrDefault(NEWTECH_CELL_XFER, false)) {
			addToProcDiagList("3E030U1", "Z006");
		}
	}

	@Override
	protected void assemblePPSVariables() {
		super.assemblePPSVariables();
		if (getProv().isFormerMdhProvider()) {
			getHoldArea().setOperHspPct(BigDecimal.ONE);
		}
	}
}