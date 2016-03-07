package com.clarify.pricer;

import java.util.HashMap;
import java.util.Map;

public class PricerResult {
	// TODO: check logic...
	// public static final int RST_FULL_DRG_PAYMENT = 0;
	// public static final int RST_DAY_OUTLIER_PAYMENT = 2;

	public static final int RST_NO_PROVIDER_INFO = 51;
	public static final int RST_INVALID_CBSA_OR_WI = 52;
	public static final int RST_PAY_CODE_NOT_ABC = 65;

	public static final Map<Integer, String> resultCodes = new HashMap<>();

	static {
		// TODO: fill codes
		resultCodes.put(RST_NO_PROVIDER_INFO,
				"NO PROVIDER SPECIFIC INFO FOUND OR PROVIDER TERMINATED");
		resultCodes.put(RST_INVALID_CBSA_OR_WI,
				"INVALID CBSA # IN PSF OR INVALID WAGE INDEX");
		resultCodes.put(RST_PAY_CODE_NOT_ABC,
				"PAY-CODE NOT = A,B OR C AND NEW HOSP NOT = Y, READMIS");
	}

	private int code;
	private String reason;

	public PricerResult() {
	}

	public PricerResult(int code) {
		setCode(code);
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

	public static PricerResult build(int code) {
		return new PricerResult(code);
	}
}