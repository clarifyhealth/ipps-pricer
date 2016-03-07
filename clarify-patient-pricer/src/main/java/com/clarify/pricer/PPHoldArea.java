package com.clarify.pricer;

import java.math.BigDecimal;

/**
 * <p>
 * Java version of PPHOLDAR as PPCAL161, only data items referenced in code.
 * </p>
 * <p>
 * A diff bewteen 161 and 154 reveal that only new field were added so this
 * should be used as baseline for newer versions.
 * </p>
 * 
 * @author ssamayoa
 *
 */
public class PPHoldArea {
	// 01 PPHOLDAR-HOLD-AREA. >> 1
	// 02 HOLD-PPS-COMPONENTS. >> 2
	// 05 H-OPER-SHARE-DOLL-THRESHOLD PIC 9(07)V9(09). >> 5
	private BigDecimal operShareDollThreshold;
	// 05 H-CAPI-SHARE-DOLL-THRESHOLD PIC 9(07)V9(09). >> 5
	private BigDecimal capiShareDollThreshold;
	// 05 H-OPER-HSP-PART PIC 9(06)V9(09). >> 10
	private BigDecimal operHspPart;
	// 05 H-CAPI-HSP-PART PIC 9(06)V9(09). >> 1
	private BigDecimal capiHspPart;
	// 05 H-OPER-FSP-PART PIC 9(06)V9(09). >> 18
	private BigDecimal operFspPart;
	// 05 H-CAPI-FSP-PART PIC 9(06)V9(09). >> 13
	private BigDecimal capiFspPart;
	// 05 H-CAPI2-B-FSP-PART PIC 9(06)V9(09). >> 2
	private BigDecimal capi2BFspPart;
	// 05 H-OPER-OUTLIER-PART PIC 9(07)V9(09). >> 6
	private BigDecimal operOutlierPart;
	// 05 H-CAPI-OUTLIER-PART PIC 9(07)V9(09). >> 6
	private BigDecimal capiOutlierPart;
	// 05 H-CAPI2-B-OUTLIER-PART PIC 9(07)V9(09). >> 5
	private BigDecimal capi2BOutlierPart;
	// 05 H-OPER-OUTDAY-PART PIC 9(07)V9(09).
	// 05 H-CAPI-OUTDAY-PART PIC 9(07)V9(09). >> 1
	private BigDecimal capiOutdayPart;
	// 05 H-OPER-OUTCST-PART PIC 9(07)V9(09). >> 8
	private BigDecimal operOutcstPart;
	// 05 H-CAPI-OUTCST-PART PIC 9(07)V9(09). >> 11
	private BigDecimal capiOutcstPart;
	// 05 H-OPER-CSTCHG-RATIO PIC 9(01)V9(03). >> 13
	private BigDecimal operCstchgRatio;
	// 05 H-CAPI-CSTCHG-RATIO PIC 9(01)V9(03). >> 10
	private BigDecimal capiCstchgRatio;
	// 05 H-OPER-IME-TEACH PIC 9(06)V9(09). >> 7
	private BigDecimal operImeTeach;
	// 05 H-CAPI-PAYCDE-PCT1 PIC 9(01)V9(02). >> 5
	private BigDecimal capiPaycdePct1;
	// 05 H-CAPI-PAYCDE-PCT2 PIC 9(01)V9(02). >> 2
	private BigDecimal capiPaycdePct2;
	// 05 H-CAPI-COST-OUTLIER PIC 9(07)V9(09). >> 6
	private BigDecimal capiCostOutlier;
	// 05 H-CAPI-BILL-COSTS PIC 9(07)V9(09). >> 5
	private BigDecimal capiBillCosts;
	// 05 H-CAPI-DOLLAR-THRESHOLD PIC 9(07)V9(09). >> 4
	private BigDecimal capiDollarThreshold;
	// 05 H-CAPI-COLA PIC 9(01)V9(03). >> 6
	private BigDecimal capiCola;
	// 05 H-CAPI-SCH PIC 9(05)V9(02). >> 3
	private BigDecimal capiSch;
	// 05 H-CAPI-BUD-NEUTRALITY PIC 9(01)V9(04).
	// 05 H-CAPI-OLD-HARMLESS PIC 9(09)V9(02). >> 13
	private BigDecimal capiOldHarmless;
	// 05 H-CAPI-FED-RATE PIC 9(05)V9(04). >> 3
	private BigDecimal capiFedRate;
	// 05 H-CAPI-FULL-PROS PIC 9(05)V9(04).
	// 05 H-CAPI-LARG-URBAN PIC 9(01)V9(02). >> 5
	private BigDecimal capiLargUrban;
	// 05 H-CAPI-GAF PIC 9(05)V9(04). >> 5
	private BigDecimal capiGaf;
	// 05 H-PR-CAPI-GAF PIC 9(05)V9(04). >> 3
	private BigDecimal prCapiGaf;
	// 05 H-BLEND-GAF PIC 9(05)V9(04).
	// 05 H-WAGE-INDEX PIC 9(02)V9(04). >> 20
	private BigDecimal wageIndex;
	// 05 H-COV-DAYS PIC 9(3). >> 3
	private int covDays;
	// 05 H-PERDIEM-DAYS PIC 9(3). >> 20
	private int perdiemDays;
	// 05 H-REG-DAYS PIC 9(3). >> 5
	private int regDays;
	// 05 H-LTR-DAYS PIC 9(3). >> 6
	private int ltrDays;
	// 05 H-DSCHG-FRCTN PIC 9(3)V9999. >> 15
	private BigDecimal dschgFrctn;
	// 05 H-DRG-WT-FRCTN PIC 9(2)V9999. >> 12
	private BigDecimal drgWtFrctn;
	// 05 H-ALOS PIC 9(02)V9(01). >> 25
	private BigDecimal alos;
	// 05 H-DAYS-CUTOFF PIC 9(02)V9(01). >> 2
	private BigDecimal daysCutoff;
	// 05 H-DAYOUT-PCT PIC 9(01)V9(02). >> 1
	private BigDecimal dayoutPct;
	// 05 H-CSTOUT-PCT PIC 9(01)V9(02). >> 6
	private BigDecimal cstoutPct;
	// 05 H-CST-THRESH PIC 9(05)V9(02). >> 18
	private BigDecimal cstThresh;
	// 05 H-OPER-BASE PIC 9(05)V9(02). >> 10
	private BigDecimal operBase;
	// 05 H-CAPI-BASE PIC 9(05)V9(02). >> 3
	private BigDecimal capiBase;
	// 05 H-OPER-BILL-STDZ-COSTS PIC 9(07)V9(02). >> 7
	private BigDecimal operBillStdzCosts;
	// 05 H-CAPI-BILL-STDZ-COSTS PIC 9(07)V9(02). >> 7
	private BigDecimal capiBillStdzCosts;
	// 05 H-OPER-STDZ-COST-OUTLIER PIC 9(07)V9(09). >> 4
	private BigDecimal operStdzCostOutlier;
	// 05 H-CAPI-STDZ-COST-OUTLIER PIC 9(07)V9(09). >> 4
	private BigDecimal capiStdzCostOutlier;
	// 05 H-OPER-STDZ-DOLLAR-THRESHOLD PIC 9(07)V9(09). >> 6
	private BigDecimal operStdzDollarThreshold;
	// 05 H-CAPI-STDZ-DOLLAR-THRESHOLD PIC 9(07)V9(02). >> 6
	private BigDecimal capiStdzDollarThreshold;
	// 05 H-STANDARD-ALLOWED-AMOUNT PIC 9(07)V9(02). >> 2
	private BigDecimal standardAllowedAmount;
	// 05 H-EHR-SUBS PIC 9(05)V9(02). >> 17
	private BigDecimal ehrSubs;
	// 05 H-PRE-CAPI-THRESH PIC 9(05)V9(02).
	// 05 H-BUDG-NUTR01 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR02 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR03 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR04 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR05 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR06 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR07 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR08 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR09 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR10 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR109 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR100 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR113 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR120 PIC 9(01)V9(06).
	// 05 H-BUDG-NUTR130 PIC 9(01)V9(06). >> 2
	private BigDecimal budgNutr130;
	// 05 H-BUDG-NUTR140 PIC 9(01)V9(06). >> 2
	private BigDecimal budgNutr140;
	// 05 H-BUDG-NUTR150 PIC 9(01)V9(06). >> 2
	private BigDecimal budgNutr150;
	// 05 H-BUDG-NUTR160 PIC 9(01)V9(06). >> 2
	private BigDecimal budgNutr160;
	// 05 H-CASE-MIX-ADJ PIC 9(01)V9(04). >> 2
	private BigDecimal caseMixAdj;
	// 05 H-SHORT-STAY-ADJ PIC 9(01)V9(04). >> 2
	private BigDecimal shortStayAdj;
	// 05 H-UPDATE-01 PIC 9(01)V9(04).
	// 05 H-UPDATE-02 PIC 9(01)V9(04).
	// 05 H-UPDATE-03 PIC 9(01)V9(04).
	// 05 H-UPDATE-04 PIC 9(01)V9(04).
	// 05 H-UPDATE-05 PIC 9(01)V9(04).
	// 05 H-UPDATE-06 PIC 9(01)V9(04).
	// 05 H-UPDATE-07 PIC 9(01)V9(04).
	// 05 H-UPDATE-08 PIC 9(01)V9(04).
	// 05 H-UPDATE-09 PIC 9(01)V9(04).
	// 05 H-UPDATE-10 PIC 9(01)V9(04).
	// 05 H-UPDATE-109 PIC 9(01)V9(04).
	// 05 H-UPDATE-113 PIC 9(01)V9(04).
	// 05 H-UPDATE-120 PIC 9(01)V9(04).
	// 05 H-UPDATE-130 PIC 9(01)V9(04). >> 2
	private BigDecimal update130;
	// 05 H-UPDATE-140 PIC 9(01)V9(05). >> 2
	private BigDecimal update140;
	// 05 H-UPDATE-150 PIC 9(01)V9(06). >> 2
	private BigDecimal update150;
	// 05 H-UPDATE-160 PIC 9(01)V9(06). >> 7
	private BigDecimal update160;
	// 05 H-ACCUM-TO-HSP PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE94 PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE95 PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE96 PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE97 PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE98 PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE99 PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE00 PIC 9(01)V9(04).
	// 05 H-HSP-UPDATE01 PIC 9(01)V9(04).
	// 05 H-PUERTO-RICO-RATE PIC 9(04)V9(02). >> 2
	private BigDecimal puertoRicoRate;
	// 05 H-FEDERAL-RATE PIC 9(04)V9(02). >> 3
	private BigDecimal federalRate;
	// 05 H-LABOR-PCT PIC 9(01)V9(04). >> 5
	private BigDecimal laborPct;
	// 05 H-NONLABOR-PCT PIC 9(01)V9(04). >> 5
	private BigDecimal nonlaborPct;
	// 05 H-PR-LABOR-PCT PIC 9(01)V9(04). >> 3
	private BigDecimal prLaborPct;
	// 05 H-PR-NONLABOR-PCT PIC 9(01)V9(04). >> 3
	private BigDecimal prNonlaborPct;
	// 05 H-HSP-RATE PIC 9(08)V9(09). >> 8
	private BigDecimal hspRate;
	// 05 H-FSP-RATE PIC 9(08)V9(09). >> 10
	private BigDecimal fspRate;
	// 05 H-OUTLIER-OFFSET-NAT PIC 9(01)V9(06).
	// 05 H-OUTLIER-OFFSET-PR PIC 9(01)V9(06).
	// 05 H-WK-OPER-DSH PIC 9(01)V9(04). >> 31
	private BigDecimal wkOperDsh;
	// 05 H-WK-CAPI-IME-TEACH PIC 9(06)V9(09). >> 4
	private BigDecimal wkCapiImeTeach;
	// 05 H-OPER-PR-DOLLAR-THRESHOLD PIC 9(07)V9(09). >> 2
	private BigDecimal operPrDollarThreshold;
	// 05 H-CAPI-PR-DOLLAR-THRESHOLD PIC 9(07)V9(09). >> 2
	private BigDecimal capiPrDollarThreshold;
	// 05 H-DSH-REDUCT-FACTOR PIC 9(01)V9(04).
	// 05 H-WK-PASS-AMT-PLUS-MISC PIC 9(06)V99. >> 2
	private BigDecimal wkPassAmtPlusMisc;
	// 05 H-BASE-DRG-PAYMENT PIC S9(07)V99. >> 11
	private BigDecimal baseDrgPayment;
	// 05 H-NEW-TECH-ADDON-CAP PIC S9(07)V99. >> 6
	private BigDecimal newTechAddonCap;
	// 05 H-NEW-TECH-ADDON-CAPDIF PIC S9(07)V99. >> 3
	private BigDecimal newTechAddonCapdif;
	// 05 H-NEW-TECH-ADDON-NEURO PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-GRAFT PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-X-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-HRTIMP-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-ISLET PIC S9(07)V99. >> 11
	private BigDecimal newTechAddonIslet;
	// 05 H-NEW-TECH-ADDON-SPIRAT-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-SPIRAT PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-AUTOLITT-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-AUTOLITT PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-DIFICID-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-DIFICID PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-ZENITH-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-ZENITH PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-VORAXAZE-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-VORAXAZE PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-ARGUS-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-ARGUS PIC S9(07)V99. >> 6
	private BigDecimal newTechAddonArgus;
	// 05 H-NEW-TECH-ADDON-KCENTRA-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-KCENTRA PIC S9(07)V99. >> 7
	private BigDecimal newTechAddonKcentra;
	// 05 H-NEW-TECH-ADDON-ZILVER-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-ZILVER PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-CARDIO-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-CARDIO PIC S9(07)V99. >> 6
	private BigDecimal newTechAddonCardio;
	// 05 H-NEW-TECH-ADDON-MITRACLP-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-MITRACLP PIC S9(07)V99. >> 6
	private BigDecimal newTechAddonMitraclp;
	// 05 H-NEW-TECH-ADDON-RNSSYS-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-RNSSYS PIC S9(07)V99. >> 7
	private BigDecimal newTechAddonRnssys;
	// 05 H-NEW-TECH-ADDON-BLINATU-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-BLINATU PIC S9(07)V99. >> 6
	private BigDecimal newTechAddonBlinatu;
	// 05 H-NEW-TECH-ADDON-LUTONIX-STOP PIC S9(07)V99.
	// 05 H-NEW-TECH-ADDON-LUTONIX PIC S9(07)V99. >> 6
	private BigDecimal newTechAddonLutonix;
	// 05 H-TECH-ADDON-ISLET-CNTR PIC S9(02). >> 54
	private BigDecimal techAddonIsletCntr;
	// 05 H-TECH-ADDON-ISLET-CNTR2 PIC S9(02). >> 1
	private BigDecimal techAddonIsletCntr2;
	// * 05 H-READMIS-ADJUST-AMT PIC S9(07)V99.
	// * 05 H-VAL-BASED-PURCH-ADJUST-AMT PIC S9(07)V99.
	// * 05 H-BUNDLE-ADJUST-AMT PIC S9(07)V99.
	// 05 H-LESSER-NEURO-1 PIC S9(07)V99.
	// 05 H-LESSER-NEURO-2 PIC S9(07)V99.
	// 05 H-LESSER-GRAFT-1 PIC S9(07)V99.
	// 05 H-LESSER-GRAFT-2 PIC S9(07)V99.
	// 05 H-LESSER-X-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-X-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-HRTIMP-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-HRTIMP-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-SPIRAT-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-SPIRAT-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-AUTOLITT-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-AUTOLITT-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-DIFICID-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-DIFICID-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-ZENITH-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-ZENITH-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-VORAXAZE-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-VORAXAZE-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-ARGUS-STOP-1 PIC S9(07)V99. >> 4
	private BigDecimal lesserArgusStop1;
	// 05 H-LESSER-ARGUS-STOP-2 PIC S9(07)V99. >> 5
	private BigDecimal lesserArgusStop2;
	// 05 H-LESSER-KCENTRA-STOP-1 PIC S9(07)V99. >> 4
	private BigDecimal lesserKcentraStop1;
	// 05 H-LESSER-KCENTRA-STOP-2 PIC S9(07)V99. >> 5
	private BigDecimal lesserKcentraStop2;
	// 05 H-LESSER-ZILVER-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-ZILVER-STOP-2 PIC S9(07)V99.
	// 05 H-LESSER-CARDIO-STOP-1 PIC S9(07)V99. >> 4
	private BigDecimal lesserCardioStop1;
	// 05 H-LESSER-CARDIO-STOP-2 PIC S9(07)V99. >> 5
	private BigDecimal lesserCardioStop2;
	// 05 H-LESSER-MITRACLP-STOP-1 PIC S9(07)V99. >> 4
	private BigDecimal lesserMitraclpStop1;
	// 05 H-LESSER-MITRACLP-STOP-2 PIC S9(07)V99. >> 5
	private BigDecimal lesserMitraclpStop2;
	// 05 H-LESSER-RNSSYS-STOP-1 PIC S9(07)V99. >> 4
	private BigDecimal lesserRnssysStop1;
	// 05 H-LESSER-RNSSYS-STOP-2 PIC S9(07)V99. >> 5
	private BigDecimal lesserRnssysStop2;
	// 05 H-LESSER-BLINATU-STOP-1 PIC S9(07)V99. >> 4
	private BigDecimal lesserBlinatuStop1;
	// 05 H-LESSER-BLINATU-STOP-2 PIC S9(07)V99. >> 5
	private BigDecimal lesserBlinatuStop2;
	// 05 H-LESSER-LUTONIX-STOP-1 PIC S9(07)V99. >> 4
	private BigDecimal lesserLutonixStop1;
	// 05 H-LESSER-LUTONIX-STOP-2 PIC S9(07)V99. >> 5
	private BigDecimal lesserLutonixStop2;
	// 05 H-LESSER-VAL-BASED-PUR-STOP-1 PIC S9(07)V99.
	// 05 H-LESSER-VAL-BASED-PUR-STOP-2 PIC S9(07)V99.
	// 05 H-CSTMED-NEURO PIC S9(07)V99.
	// 05 H-CSTMED-GRAFT PIC S9(07)V99.
	// 05 H-CSTMED-X-STOP PIC S9(07)V99.
	// 05 H-CSTMED-HRTIMP-STOP PIC S9(07)V99.
	// 05 H-CSTMED-SPIRAT-STOP PIC S9(07)V99.
	// 05 H-CSTMED-AUTOLITT-STOP PIC S9(07)V99.
	// 05 H-CSTMED-DIFICID-STOP PIC S9(07)V99.
	// 05 H-CSTMED-ZENITH-STOP PIC S9(07)V99.
	// 05 H-CSTMED-VORAXAZE-STOP PIC S9(07)V99.
	// 05 H-CSTMED-ARGUS-STOP PIC S9(07)V99. >> 3
	private BigDecimal cstmedArgusStop;
	// 05 H-CSTMED-KCENTRA-STOP PIC S9(07)V99. >> 3
	private BigDecimal cstmedKcentraStop;
	// 05 H-CSTMED-ZILVER-STOP PIC S9(07)V99.
	// 05 H-CSTMED-CARDIO-STOP PIC S9(07)V99. >> 3
	private BigDecimal cstmedCardioStop;
	// 05 H-CSTMED-MITRACLP-STOP PIC S9(07)V99. >> 3
	private BigDecimal cstmedMitraclpStop;
	// 05 H-CSTMED-RNSSYS-STOP PIC S9(07)V99. >> 3
	private BigDecimal cstmedRnssysStop;
	// 05 H-CSTMED-BLINATU-STOP PIC S9(07)V99. >> 3
	private BigDecimal cstmedBlinatuStop;
	// 05 H-CSTMED-LUTONIX-STOP PIC S9(07)V99. >> 3
	private BigDecimal cstmedLutonixStop;
	// 02 HOLD-ADDITIONAL-VARIABLES. >> 4
	// 05 H-OPER-HSP-PCT PIC 9(01)V9(02). >> 4
	private BigDecimal operHspPct;
	// 05 H-OPER-FSP-PCT PIC 9(01)V9(02). >> 4
	private BigDecimal operFspPct;
	// 05 H-NAT-PCT PIC 9(01)V9(02). >> 9
	private BigDecimal natPct;
	// 05 H-REG-PCT PIC 9(01)V9(02). >> 7
	private BigDecimal regPct;
	// 05 H-FAC-SPEC-RATE PIC 9(05)V9(02). >> 2
	private BigDecimal facSpecRate;
	// 05 H-UPDATE-FACTOR PIC 9(01)V9(05). >> 2
	private BigDecimal updateFactor;
	// 05 H-DRG-WT PIC 9(02)V9(04). >> 20
	private BigDecimal drgWt;
	// 05 H-NAT-LABOR PIC 9(05)V9(02). >> 12
	private BigDecimal natLabor;
	// 05 H-NAT-NONLABOR PIC 9(05)V9(02). >> 12
	private BigDecimal natNonlabor;
	// 05 H-REG-LABOR PIC 9(05)V9(02). >> 12
	private BigDecimal regLabor;
	// 05 H-REG-NONLABOR PIC 9(05)V9(02). >> 12
	private BigDecimal regNonlabor;
	// 05 H-OPER-COLA PIC 9(01)V9(03). >> 13
	private BigDecimal operCola;
	// 05 H-INTERN-RATIO PIC 9(01)V9(04). >> 2
	private BigDecimal internRatio;
	// 05 H-OPER-COST-OUTLIER PIC 9(07)V9(09). >> 5
	private BigDecimal operCostOutlier;
	// 05 H-OPER-BILL-COSTS PIC 9(07)V9(09). >> 10
	private BigDecimal operBillCosts;
	// 05 H-OPER-DOLLAR-THRESHOLD PIC 9(07)V9(09). >> 13
	private BigDecimal operDollarThreshold;
	// 02 HOLD-CAPITAL-VARIABLES. >> 5
	// 05 H-CAPI-TOTAL-PAY PIC 9(07)V9(02). >> 2
	private BigDecimal capiTotalPay;
	// 05 H-CAPI-HSP PIC 9(07)V9(02). >> 4
	private BigDecimal capiHsp;
	// 05 H-CAPI-FSP PIC 9(07)V9(02). >> 20
	private BigDecimal capiFsp;
	// 05 H-CAPI-OUTLIER PIC 9(07)V9(02). >> 10
	private BigDecimal capiOutlier;
	// 05 H-CAPI-OLD-HARM PIC 9(07)V9(02). >> 13
	private BigDecimal capiOldHarm;
	// 05 H-CAPI-DSH-ADJ PIC 9(07)V9(02). >> 3
	private BigDecimal capiDshAdj;
	// 05 H-CAPI-IME-ADJ PIC 9(07)V9(02). >> 3
	private BigDecimal capiImeAdj;
	// 05 H-CAPI-EXCEPTIONS PIC 9(07)V9(02). >> 1
	private BigDecimal capiExceptions;
	// 02 HOLD-CAPITAL2-VARIABLES. >> 4
	// 05 H-CAPI2-PAY-CODE PIC X(1). >> 1
	private String capi2PayCode;
	// 05 H-CAPI2-B-FSP PIC 9(07)V9(02). >> 3
	private BigDecimal capi2BFsp;
	// 05 H-CAPI2-B-OUTLIER PIC 9(07)V9(02). >> 6
	private BigDecimal capi2BOutlier;
	// 02 HOLD-OTHER-VARIABLES. >> 4
	// 05 H-NON-TEMP-RELIEF-PAYMENT PIC 9(07)V9(02).
	// 05 H-NEW-TECH-PAY-ADD-ON PIC 9(07)V9(02). >> 24
	private BigDecimal newTechPayAddOn;
	// 05 H-LOW-VOL-PAYMENT PIC 9(07)V9(02). >> 1
	private BigDecimal lowVolPayment;
	// 05 H-VAL-BASED-PURCH-PARTIPNT PIC X. >> 1
	private String valBasedPurchPartipnt;
	// 05 H-VAL-BASED-PURCH-ADJUST PIC 9V9(11). >> 8
	private BigDecimal valBasedPurchAdjust;
	// 05 H-HOSP-READMISSION-REDU PIC X. >> 1
	private String hospReadmissionRedu;
	// 05 H-HOSP-HRR-ADJUSTMT PIC 9V9(4). >> 1
	private BigDecimal hospHrrAdjustmt;
	// 05 H-OPERATNG-DATA.
	// 10 H-MODEL1-BUNDLE-DISPRCNT PIC V999.
	// 10 H-OPER-BASE-DRG-PAY PIC 9(08)V99. >> 7
	private BigDecimal operBaseDrgPay;
	// 10 H-OPER-HSP-AMT PIC 9(08)V99.
	// 02 HOLD-PC-OTH-VARIABLES. >> 4
	// 05 H-OPER-DSH PIC 9(01)V9(04). >> 36
	private BigDecimal operDsh;
	// 05 H-CAPI-DSH PIC 9(01)V9(04). >> 7
	private BigDecimal capiDsh;
	// 05 H-CAPI-HSP-PCT PIC 9(01)V9(02). >> 3
	private BigDecimal capiHspPct;
	// 05 H-CAPI-FSP-PCT PIC 9(01)V9(04). >> 3
	private BigDecimal capiFspPct;
	// 05 H-ARITH-ALOS PIC 9(02)V9(01). >> 1
	private BigDecimal arithAlos;
	// 05 H-PR-WAGE-INDEX PIC 9(02)V9(04). >> 10
	private BigDecimal prWageIndex;
	// 05 H-TRANSFER-ADJ PIC 9(01)V9(04). >> 13
	private BigDecimal transferAdj;
	// 05 H-PC-HMO-FLAG PIC X(01).
	// 05 H-PC-COT-FLAG PIC X(01).
	// 05 H-OPER-HSP-PART2 PIC 9(07)V9(02).
	// 05 H-BUNDLE-ADJUST-PAY PIC S9(07)V99.
	// 02 H-ADDITIONAL-PAY-INFO-DATA. >> 8
	// 05 H-UNCOMP-CARE-AMOUNT PIC S9(07)V9(02). >> 1
	private BigDecimal uncompCareAmount;
	// 05 H-BUNDLE-ADJUST-AMT PIC S9(07)V9(02). >> 9
	private BigDecimal bundleAdjustAmt;
	// 05 H-VAL-BASED-PURCH-ADJUST-AMT PIC S9(07)V9(02). >> 7
	private BigDecimal valBasedPurchAdjustAmt;
	// 05 H-READMIS-ADJUST-AMT PIC S9(07)V9(02). >> 12
	private BigDecimal readmisAdjustAmt;
	// 02 H-ADDITIONAL-PAY-INFO-DATA2. >> 4
	// 05 H-HAC-PROG-REDUC-IND PIC X.
	// 05 H-EHR-PROG-REDUC-IND PIC X.
	// 05 H-EHR-ADJUST-AMT PIC S9(07)V9(02). >> 2
	private BigDecimal ehrAdjustAmt;

	// 05 H-STNDRD-VALUE PIC S9(07)V9(02).
	// 05 H-HAC-PAYMENT-AMT PIC S9(07)V9(02).
	// 05 H-FLX7-PAYMENT PIC S9(07)V9(02).
	// 02 H-FILLER PIC X(0906).

	public BigDecimal getOperShareDollThreshold() {
		return operShareDollThreshold;
	}

	public void setOperShareDollThreshold(BigDecimal operShareDollThreshold) {
		this.operShareDollThreshold = operShareDollThreshold;
	}

	public BigDecimal getCapiShareDollThreshold() {
		return capiShareDollThreshold;
	}

	public void setCapiShareDollThreshold(BigDecimal capiShareDollThreshold) {
		this.capiShareDollThreshold = capiShareDollThreshold;
	}

	public BigDecimal getOperHspPart() {
		return operHspPart;
	}

	public void setOperHspPart(BigDecimal operHspPart) {
		this.operHspPart = operHspPart;
	}

	public BigDecimal getCapiHspPart() {
		return capiHspPart;
	}

	public void setCapiHspPart(BigDecimal capiHspPart) {
		this.capiHspPart = capiHspPart;
	}

	public BigDecimal getOperFspPart() {
		return operFspPart;
	}

	public void setOperFspPart(BigDecimal operFspPart) {
		this.operFspPart = operFspPart;
	}

	public BigDecimal getCapiFspPart() {
		return capiFspPart;
	}

	public void setCapiFspPart(BigDecimal capiFspPart) {
		this.capiFspPart = capiFspPart;
	}

	public BigDecimal getCapi2BFspPart() {
		return capi2BFspPart;
	}

	public void setCapi2BFspPart(BigDecimal capi2bFspPart) {
		capi2BFspPart = capi2bFspPart;
	}

	public BigDecimal getOperOutlierPart() {
		return operOutlierPart;
	}

	public void setOperOutlierPart(BigDecimal operOutlierPart) {
		this.operOutlierPart = operOutlierPart;
	}

	public BigDecimal getCapiOutlierPart() {
		return capiOutlierPart;
	}

	public void setCapiOutlierPart(BigDecimal capiOutlierPart) {
		this.capiOutlierPart = capiOutlierPart;
	}

	public BigDecimal getCapi2BOutlierPart() {
		return capi2BOutlierPart;
	}

	public void setCapi2BOutlierPart(BigDecimal capi2bOutlierPart) {
		capi2BOutlierPart = capi2bOutlierPart;
	}

	public BigDecimal getCapiOutdayPart() {
		return capiOutdayPart;
	}

	public void setCapiOutdayPart(BigDecimal capiOutdayPart) {
		this.capiOutdayPart = capiOutdayPart;
	}

	public BigDecimal getOperOutcstPart() {
		return operOutcstPart;
	}

	public void setOperOutcstPart(BigDecimal operOutcstPart) {
		this.operOutcstPart = operOutcstPart;
	}

	public BigDecimal getCapiOutcstPart() {
		return capiOutcstPart;
	}

	public void setCapiOutcstPart(BigDecimal capiOutcstPart) {
		this.capiOutcstPart = capiOutcstPart;
	}

	public BigDecimal getOperCstchgRatio() {
		return operCstchgRatio;
	}

	public void setOperCstchgRatio(BigDecimal operCstchgRatio) {
		this.operCstchgRatio = operCstchgRatio;
	}

	public BigDecimal getCapiCstchgRatio() {
		return capiCstchgRatio;
	}

	public void setCapiCstchgRatio(BigDecimal capiCstchgRatio) {
		this.capiCstchgRatio = capiCstchgRatio;
	}

	public BigDecimal getOperImeTeach() {
		return operImeTeach;
	}

	public void setOperImeTeach(BigDecimal operImeTeach) {
		this.operImeTeach = operImeTeach;
	}

	public BigDecimal getCapiPaycdePct1() {
		return capiPaycdePct1;
	}

	public void setCapiPaycdePct1(BigDecimal capiPaycdePct1) {
		this.capiPaycdePct1 = capiPaycdePct1;
	}

	public BigDecimal getCapiPaycdePct2() {
		return capiPaycdePct2;
	}

	public void setCapiPaycdePct2(BigDecimal capiPaycdePct2) {
		this.capiPaycdePct2 = capiPaycdePct2;
	}

	public BigDecimal getCapiCostOutlier() {
		return capiCostOutlier;
	}

	public void setCapiCostOutlier(BigDecimal capiCostOutlier) {
		this.capiCostOutlier = capiCostOutlier;
	}

	public BigDecimal getCapiBillCosts() {
		return capiBillCosts;
	}

	public void setCapiBillCosts(BigDecimal capiBillCosts) {
		this.capiBillCosts = capiBillCosts;
	}

	public BigDecimal getCapiDollarThreshold() {
		return capiDollarThreshold;
	}

	public void setCapiDollarThreshold(BigDecimal capiDollarThreshold) {
		this.capiDollarThreshold = capiDollarThreshold;
	}

	public BigDecimal getCapiCola() {
		return capiCola;
	}

	public void setCapiCola(BigDecimal capiCola) {
		this.capiCola = capiCola;
	}

	public BigDecimal getCapiSch() {
		return capiSch;
	}

	public void setCapiSch(BigDecimal capiSch) {
		this.capiSch = capiSch;
	}

	public BigDecimal getCapiOldHarmless() {
		return capiOldHarmless;
	}

	public void setCapiOldHarmless(BigDecimal capiOldHarmless) {
		this.capiOldHarmless = capiOldHarmless;
	}

	public BigDecimal getCapiFedRate() {
		return capiFedRate;
	}

	public void setCapiFedRate(BigDecimal capiFedRate) {
		this.capiFedRate = capiFedRate;
	}

	public BigDecimal getCapiLargUrban() {
		return capiLargUrban;
	}

	public void setCapiLargUrban(BigDecimal capiLargUrban) {
		this.capiLargUrban = capiLargUrban;
	}

	public BigDecimal getCapiGaf() {
		return capiGaf;
	}

	public void setCapiGaf(BigDecimal capiGaf) {
		this.capiGaf = capiGaf;
	}

	public BigDecimal getPrCapiGaf() {
		return prCapiGaf;
	}

	public void setPrCapiGaf(BigDecimal prCapiGaf) {
		this.prCapiGaf = prCapiGaf;
	}

	public BigDecimal getWageIndex() {
		return wageIndex;
	}

	public void setWageIndex(BigDecimal wageIndex) {
		this.wageIndex = wageIndex;
	}

	public int getCovDays() {
		return covDays;
	}

	public void setCovDays(int covDays) {
		this.covDays = covDays;
	}

	public int getPerdiemDays() {
		return perdiemDays;
	}

	public void setPerdiemDays(int perdiemDays) {
		this.perdiemDays = perdiemDays;
	}

	public int getRegDays() {
		return regDays;
	}

	public void setRegDays(int regDays) {
		this.regDays = regDays;
	}

	public int getLtrDays() {
		return ltrDays;
	}

	public void setLtrDays(int ltrDays) {
		this.ltrDays = ltrDays;
	}

	public BigDecimal getDschgFrctn() {
		return dschgFrctn;
	}

	public void setDschgFrctn(BigDecimal dschgFrctn) {
		this.dschgFrctn = dschgFrctn;
	}

	public BigDecimal getDrgWtFrctn() {
		return drgWtFrctn;
	}

	public void setDrgWtFrctn(BigDecimal drgWtFrctn) {
		this.drgWtFrctn = drgWtFrctn;
	}

	public BigDecimal getAlos() {
		return alos;
	}

	public void setAlos(BigDecimal alos) {
		this.alos = alos;
	}

	public BigDecimal getDaysCutoff() {
		return daysCutoff;
	}

	public void setDaysCutoff(BigDecimal daysCutoff) {
		this.daysCutoff = daysCutoff;
	}

	public BigDecimal getDayoutPct() {
		return dayoutPct;
	}

	public void setDayoutPct(BigDecimal dayoutPct) {
		this.dayoutPct = dayoutPct;
	}

	public BigDecimal getCstoutPct() {
		return cstoutPct;
	}

	public void setCstoutPct(BigDecimal cstoutPct) {
		this.cstoutPct = cstoutPct;
	}

	public BigDecimal getCstThresh() {
		return cstThresh;
	}

	public void setCstThresh(BigDecimal cstThresh) {
		this.cstThresh = cstThresh;
	}

	public BigDecimal getOperBase() {
		return operBase;
	}

	public void setOperBase(BigDecimal operBase) {
		this.operBase = operBase;
	}

	public BigDecimal getCapiBase() {
		return capiBase;
	}

	public void setCapiBase(BigDecimal capiBase) {
		this.capiBase = capiBase;
	}

	public BigDecimal getOperBillStdzCosts() {
		return operBillStdzCosts;
	}

	public void setOperBillStdzCosts(BigDecimal operBillStdzCosts) {
		this.operBillStdzCosts = operBillStdzCosts;
	}

	public BigDecimal getCapiBillStdzCosts() {
		return capiBillStdzCosts;
	}

	public void setCapiBillStdzCosts(BigDecimal capiBillStdzCosts) {
		this.capiBillStdzCosts = capiBillStdzCosts;
	}

	public BigDecimal getOperStdzCostOutlier() {
		return operStdzCostOutlier;
	}

	public void setOperStdzCostOutlier(BigDecimal operStdzCostOutlier) {
		this.operStdzCostOutlier = operStdzCostOutlier;
	}

	public BigDecimal getCapiStdzCostOutlier() {
		return capiStdzCostOutlier;
	}

	public void setCapiStdzCostOutlier(BigDecimal capiStdzCostOutlier) {
		this.capiStdzCostOutlier = capiStdzCostOutlier;
	}

	public BigDecimal getOperStdzDollarThreshold() {
		return operStdzDollarThreshold;
	}

	public void setOperStdzDollarThreshold(BigDecimal operStdzDollarThreshold) {
		this.operStdzDollarThreshold = operStdzDollarThreshold;
	}

	public BigDecimal getCapiStdzDollarThreshold() {
		return capiStdzDollarThreshold;
	}

	public void setCapiStdzDollarThreshold(BigDecimal capiStdzDollarThreshold) {
		this.capiStdzDollarThreshold = capiStdzDollarThreshold;
	}

	public BigDecimal getStandardAllowedAmount() {
		return standardAllowedAmount;
	}

	public void setStandardAllowedAmount(BigDecimal standardAllowedAmount) {
		this.standardAllowedAmount = standardAllowedAmount;
	}

	public BigDecimal getEhrSubs() {
		return ehrSubs;
	}

	public void setEhrSubs(BigDecimal ehrSubs) {
		this.ehrSubs = ehrSubs;
	}

	public BigDecimal getBudgNutr130() {
		return budgNutr130;
	}

	public void setBudgNutr130(BigDecimal budgNutr130) {
		this.budgNutr130 = budgNutr130;
	}

	public BigDecimal getBudgNutr140() {
		return budgNutr140;
	}

	public void setBudgNutr140(BigDecimal budgNutr140) {
		this.budgNutr140 = budgNutr140;
	}

	public BigDecimal getBudgNutr150() {
		return budgNutr150;
	}

	public void setBudgNutr150(BigDecimal budgNutr150) {
		this.budgNutr150 = budgNutr150;
	}

	public BigDecimal getBudgNutr160() {
		return budgNutr160;
	}

	public void setBudgNutr160(BigDecimal budgNutr160) {
		this.budgNutr160 = budgNutr160;
	}

	public BigDecimal getCaseMixAdj() {
		return caseMixAdj;
	}

	public void setCaseMixAdj(BigDecimal caseMixAdj) {
		this.caseMixAdj = caseMixAdj;
	}

	public BigDecimal getShortStayAdj() {
		return shortStayAdj;
	}

	public void setShortStayAdj(BigDecimal shortStayAdj) {
		this.shortStayAdj = shortStayAdj;
	}

	public BigDecimal getUpdate130() {
		return update130;
	}

	public void setUpdate130(BigDecimal update130) {
		this.update130 = update130;
	}

	public BigDecimal getUpdate140() {
		return update140;
	}

	public void setUpdate140(BigDecimal update140) {
		this.update140 = update140;
	}

	public BigDecimal getUpdate150() {
		return update150;
	}

	public void setUpdate150(BigDecimal update150) {
		this.update150 = update150;
	}

	public BigDecimal getUpdate160() {
		return update160;
	}

	public void setUpdate160(BigDecimal update160) {
		this.update160 = update160;
	}

	public BigDecimal getPuertoRicoRate() {
		return puertoRicoRate;
	}

	public void setPuertoRicoRate(BigDecimal puertoRicoRate) {
		this.puertoRicoRate = puertoRicoRate;
	}

	public BigDecimal getFederalRate() {
		return federalRate;
	}

	public void setFederalRate(BigDecimal federalRate) {
		this.federalRate = federalRate;
	}

	public BigDecimal getLaborPct() {
		return laborPct;
	}

	public void setLaborPct(BigDecimal laborPct) {
		this.laborPct = laborPct;
	}

	public BigDecimal getNonlaborPct() {
		return nonlaborPct;
	}

	public void setNonlaborPct(BigDecimal nonlaborPct) {
		this.nonlaborPct = nonlaborPct;
	}

	public BigDecimal getPrLaborPct() {
		return prLaborPct;
	}

	public void setPrLaborPct(BigDecimal prLaborPct) {
		this.prLaborPct = prLaborPct;
	}

	public BigDecimal getPrNonlaborPct() {
		return prNonlaborPct;
	}

	public void setPrNonlaborPct(BigDecimal prNonlaborPct) {
		this.prNonlaborPct = prNonlaborPct;
	}

	public BigDecimal getHspRate() {
		return hspRate;
	}

	public void setHspRate(BigDecimal hspRate) {
		this.hspRate = hspRate;
	}

	public BigDecimal getFspRate() {
		return fspRate;
	}

	public void setFspRate(BigDecimal fspRate) {
		this.fspRate = fspRate;
	}

	public BigDecimal getWkOperDsh() {
		return wkOperDsh;
	}

	public void setWkOperDsh(BigDecimal wkOperDsh) {
		this.wkOperDsh = wkOperDsh;
	}

	public BigDecimal getWkCapiImeTeach() {
		return wkCapiImeTeach;
	}

	public void setWkCapiImeTeach(BigDecimal wkCapiImeTeach) {
		this.wkCapiImeTeach = wkCapiImeTeach;
	}

	public BigDecimal getOperPrDollarThreshold() {
		return operPrDollarThreshold;
	}

	public void setOperPrDollarThreshold(BigDecimal operPrDollarThreshold) {
		this.operPrDollarThreshold = operPrDollarThreshold;
	}

	public BigDecimal getCapiPrDollarThreshold() {
		return capiPrDollarThreshold;
	}

	public void setCapiPrDollarThreshold(BigDecimal capiPrDollarThreshold) {
		this.capiPrDollarThreshold = capiPrDollarThreshold;
	}

	public BigDecimal getWkPassAmtPlusMisc() {
		return wkPassAmtPlusMisc;
	}

	public void setWkPassAmtPlusMisc(BigDecimal wkPassAmtPlusMisc) {
		this.wkPassAmtPlusMisc = wkPassAmtPlusMisc;
	}

	public BigDecimal getBaseDrgPayment() {
		return baseDrgPayment;
	}

	public void setBaseDrgPayment(BigDecimal baseDrgPayment) {
		this.baseDrgPayment = baseDrgPayment;
	}

	public BigDecimal getNewTechAddonCap() {
		return newTechAddonCap;
	}

	public void setNewTechAddonCap(BigDecimal newTechAddonCap) {
		this.newTechAddonCap = newTechAddonCap;
	}

	public BigDecimal getNewTechAddonCapdif() {
		return newTechAddonCapdif;
	}

	public void setNewTechAddonCapdif(BigDecimal newTechAddonCapdif) {
		this.newTechAddonCapdif = newTechAddonCapdif;
	}

	public BigDecimal getNewTechAddonIslet() {
		return newTechAddonIslet;
	}

	public void setNewTechAddonIslet(BigDecimal newTechAddonIslet) {
		this.newTechAddonIslet = newTechAddonIslet;
	}

	public BigDecimal getNewTechAddonArgus() {
		return newTechAddonArgus;
	}

	public void setNewTechAddonArgus(BigDecimal newTechAddonArgus) {
		this.newTechAddonArgus = newTechAddonArgus;
	}

	public BigDecimal getNewTechAddonKcentra() {
		return newTechAddonKcentra;
	}

	public void setNewTechAddonKcentra(BigDecimal newTechAddonKcentra) {
		this.newTechAddonKcentra = newTechAddonKcentra;
	}

	public BigDecimal getNewTechAddonCardio() {
		return newTechAddonCardio;
	}

	public void setNewTechAddonCardio(BigDecimal newTechAddonCardio) {
		this.newTechAddonCardio = newTechAddonCardio;
	}

	public BigDecimal getNewTechAddonMitraclp() {
		return newTechAddonMitraclp;
	}

	public void setNewTechAddonMitraclp(BigDecimal newTechAddonMitraclp) {
		this.newTechAddonMitraclp = newTechAddonMitraclp;
	}

	public BigDecimal getNewTechAddonRnssys() {
		return newTechAddonRnssys;
	}

	public void setNewTechAddonRnssys(BigDecimal newTechAddonRnssys) {
		this.newTechAddonRnssys = newTechAddonRnssys;
	}

	public BigDecimal getNewTechAddonBlinatu() {
		return newTechAddonBlinatu;
	}

	public void setNewTechAddonBlinatu(BigDecimal newTechAddonBlinatu) {
		this.newTechAddonBlinatu = newTechAddonBlinatu;
	}

	public BigDecimal getNewTechAddonLutonix() {
		return newTechAddonLutonix;
	}

	public void setNewTechAddonLutonix(BigDecimal newTechAddonLutonix) {
		this.newTechAddonLutonix = newTechAddonLutonix;
	}

	public BigDecimal getTechAddonIsletCntr() {
		return techAddonIsletCntr;
	}

	public void setTechAddonIsletCntr(BigDecimal techAddonIsletCntr) {
		this.techAddonIsletCntr = techAddonIsletCntr;
	}

	public BigDecimal getTechAddonIsletCntr2() {
		return techAddonIsletCntr2;
	}

	public void setTechAddonIsletCntr2(BigDecimal techAddonIsletCntr2) {
		this.techAddonIsletCntr2 = techAddonIsletCntr2;
	}

	public BigDecimal getLesserArgusStop1() {
		return lesserArgusStop1;
	}

	public void setLesserArgusStop1(BigDecimal lesserArgusStop1) {
		this.lesserArgusStop1 = lesserArgusStop1;
	}

	public BigDecimal getLesserArgusStop2() {
		return lesserArgusStop2;
	}

	public void setLesserArgusStop2(BigDecimal lesserArgusStop2) {
		this.lesserArgusStop2 = lesserArgusStop2;
	}

	public BigDecimal getLesserKcentraStop1() {
		return lesserKcentraStop1;
	}

	public void setLesserKcentraStop1(BigDecimal lesserKcentraStop1) {
		this.lesserKcentraStop1 = lesserKcentraStop1;
	}

	public BigDecimal getLesserKcentraStop2() {
		return lesserKcentraStop2;
	}

	public void setLesserKcentraStop2(BigDecimal lesserKcentraStop2) {
		this.lesserKcentraStop2 = lesserKcentraStop2;
	}

	public BigDecimal getLesserCardioStop1() {
		return lesserCardioStop1;
	}

	public void setLesserCardioStop1(BigDecimal lesserCardioStop1) {
		this.lesserCardioStop1 = lesserCardioStop1;
	}

	public BigDecimal getLesserCardioStop2() {
		return lesserCardioStop2;
	}

	public void setLesserCardioStop2(BigDecimal lesserCardioStop2) {
		this.lesserCardioStop2 = lesserCardioStop2;
	}

	public BigDecimal getLesserMitraclpStop1() {
		return lesserMitraclpStop1;
	}

	public void setLesserMitraclpStop1(BigDecimal lesserMitraclpStop1) {
		this.lesserMitraclpStop1 = lesserMitraclpStop1;
	}

	public BigDecimal getLesserMitraclpStop2() {
		return lesserMitraclpStop2;
	}

	public void setLesserMitraclpStop2(BigDecimal lesserMitraclpStop2) {
		this.lesserMitraclpStop2 = lesserMitraclpStop2;
	}

	public BigDecimal getLesserRnssysStop1() {
		return lesserRnssysStop1;
	}

	public void setLesserRnssysStop1(BigDecimal lesserRnssysStop1) {
		this.lesserRnssysStop1 = lesserRnssysStop1;
	}

	public BigDecimal getLesserRnssysStop2() {
		return lesserRnssysStop2;
	}

	public void setLesserRnssysStop2(BigDecimal lesserRnssysStop2) {
		this.lesserRnssysStop2 = lesserRnssysStop2;
	}

	public BigDecimal getLesserBlinatuStop1() {
		return lesserBlinatuStop1;
	}

	public void setLesserBlinatuStop1(BigDecimal lesserBlinatuStop1) {
		this.lesserBlinatuStop1 = lesserBlinatuStop1;
	}

	public BigDecimal getLesserBlinatuStop2() {
		return lesserBlinatuStop2;
	}

	public void setLesserBlinatuStop2(BigDecimal lesserBlinatuStop2) {
		this.lesserBlinatuStop2 = lesserBlinatuStop2;
	}

	public BigDecimal getLesserLutonixStop1() {
		return lesserLutonixStop1;
	}

	public void setLesserLutonixStop1(BigDecimal lesserLutonixStop1) {
		this.lesserLutonixStop1 = lesserLutonixStop1;
	}

	public BigDecimal getLesserLutonixStop2() {
		return lesserLutonixStop2;
	}

	public void setLesserLutonixStop2(BigDecimal lesserLutonixStop2) {
		this.lesserLutonixStop2 = lesserLutonixStop2;
	}

	public BigDecimal getCstmedArgusStop() {
		return cstmedArgusStop;
	}

	public void setCstmedArgusStop(BigDecimal cstmedArgusStop) {
		this.cstmedArgusStop = cstmedArgusStop;
	}

	public BigDecimal getCstmedKcentraStop() {
		return cstmedKcentraStop;
	}

	public void setCstmedKcentraStop(BigDecimal cstmedKcentraStop) {
		this.cstmedKcentraStop = cstmedKcentraStop;
	}

	public BigDecimal getCstmedCardioStop() {
		return cstmedCardioStop;
	}

	public void setCstmedCardioStop(BigDecimal cstmedCardioStop) {
		this.cstmedCardioStop = cstmedCardioStop;
	}

	public BigDecimal getCstmedMitraclpStop() {
		return cstmedMitraclpStop;
	}

	public void setCstmedMitraclpStop(BigDecimal cstmedMitraclpStop) {
		this.cstmedMitraclpStop = cstmedMitraclpStop;
	}

	public BigDecimal getCstmedRnssysStop() {
		return cstmedRnssysStop;
	}

	public void setCstmedRnssysStop(BigDecimal cstmedRnssysStop) {
		this.cstmedRnssysStop = cstmedRnssysStop;
	}

	public BigDecimal getCstmedBlinatuStop() {
		return cstmedBlinatuStop;
	}

	public void setCstmedBlinatuStop(BigDecimal cstmedBlinatuStop) {
		this.cstmedBlinatuStop = cstmedBlinatuStop;
	}

	public BigDecimal getCstmedLutonixStop() {
		return cstmedLutonixStop;
	}

	public void setCstmedLutonixStop(BigDecimal cstmedLutonixStop) {
		this.cstmedLutonixStop = cstmedLutonixStop;
	}

	public BigDecimal getOperHspPct() {
		return operHspPct;
	}

	public void setOperHspPct(BigDecimal operHspPct) {
		this.operHspPct = operHspPct;
	}

	public BigDecimal getOperFspPct() {
		return operFspPct;
	}

	public void setOperFspPct(BigDecimal operFspPct) {
		this.operFspPct = operFspPct;
	}

	public BigDecimal getNatPct() {
		return natPct;
	}

	public void setNatPct(BigDecimal natPct) {
		this.natPct = natPct;
	}

	public void setNatPct(double natPct) {
		this.natPct = BigDecimal.valueOf(natPct);
	}

	public BigDecimal getRegPct() {
		return regPct;
	}

	public void setRegPct(BigDecimal regPct) {
		this.regPct = regPct;
	}

	public void setRegPct(double regPct) {
		this.regPct = BigDecimal.valueOf(regPct);
	}

	public BigDecimal getFacSpecRate() {
		return facSpecRate;
	}

	public void setFacSpecRate(BigDecimal facSpecRate) {
		this.facSpecRate = facSpecRate;
	}

	public BigDecimal getUpdateFactor() {
		return updateFactor;
	}

	public void setUpdateFactor(BigDecimal updateFactor) {
		this.updateFactor = updateFactor;
	}

	public BigDecimal getDrgWt() {
		return drgWt;
	}

	public void setDrgWt(BigDecimal drgWt) {
		this.drgWt = drgWt;
	}

	public BigDecimal getNatLabor() {
		return natLabor;
	}

	public void setNatLabor(BigDecimal natLabor) {
		this.natLabor = natLabor;
	}

	public BigDecimal getNatNonlabor() {
		return natNonlabor;
	}

	public void setNatNonlabor(BigDecimal natNonlabor) {
		this.natNonlabor = natNonlabor;
	}

	public BigDecimal getRegLabor() {
		return regLabor;
	}

	public void setRegLabor(BigDecimal regLabor) {
		this.regLabor = regLabor;
	}

	public BigDecimal getRegNonlabor() {
		return regNonlabor;
	}

	public void setRegNonlabor(BigDecimal regNonlabor) {
		this.regNonlabor = regNonlabor;
	}

	public BigDecimal getOperCola() {
		return operCola;
	}

	public void setOperCola(BigDecimal operCola) {
		this.operCola = operCola;
	}

	public BigDecimal getInternRatio() {
		return internRatio;
	}

	public void setInternRatio(BigDecimal internRatio) {
		this.internRatio = internRatio;
	}

	public BigDecimal getOperCostOutlier() {
		return operCostOutlier;
	}

	public void setOperCostOutlier(BigDecimal operCostOutlier) {
		this.operCostOutlier = operCostOutlier;
	}

	public BigDecimal getOperBillCosts() {
		return operBillCosts;
	}

	public void setOperBillCosts(BigDecimal operBillCosts) {
		this.operBillCosts = operBillCosts;
	}

	public BigDecimal getOperDollarThreshold() {
		return operDollarThreshold;
	}

	public void setOperDollarThreshold(BigDecimal operDollarThreshold) {
		this.operDollarThreshold = operDollarThreshold;
	}

	public BigDecimal getCapiTotalPay() {
		return capiTotalPay;
	}

	public void setCapiTotalPay(BigDecimal capiTotalPay) {
		this.capiTotalPay = capiTotalPay;
	}

	public BigDecimal getCapiHsp() {
		return capiHsp;
	}

	public void setCapiHsp(BigDecimal capiHsp) {
		this.capiHsp = capiHsp;
	}

	public BigDecimal getCapiFsp() {
		return capiFsp;
	}

	public void setCapiFsp(BigDecimal capiFsp) {
		this.capiFsp = capiFsp;
	}

	public BigDecimal getCapiOutlier() {
		return capiOutlier;
	}

	public void setCapiOutlier(BigDecimal capiOutlier) {
		this.capiOutlier = capiOutlier;
	}

	public BigDecimal getCapiOldHarm() {
		return capiOldHarm;
	}

	public void setCapiOldHarm(BigDecimal capiOldHarm) {
		this.capiOldHarm = capiOldHarm;
	}

	public BigDecimal getCapiDshAdj() {
		return capiDshAdj;
	}

	public void setCapiDshAdj(BigDecimal capiDshAdj) {
		this.capiDshAdj = capiDshAdj;
	}

	public BigDecimal getCapiImeAdj() {
		return capiImeAdj;
	}

	public void setCapiImeAdj(BigDecimal capiImeAdj) {
		this.capiImeAdj = capiImeAdj;
	}

	public BigDecimal getCapiExceptions() {
		return capiExceptions;
	}

	public void setCapiExceptions(BigDecimal capiExceptions) {
		this.capiExceptions = capiExceptions;
	}

	public String getCapi2PayCode() {
		return capi2PayCode;
	}

	public void setCapi2PayCode(String capi2PayCode) {
		this.capi2PayCode = capi2PayCode;
	}

	public BigDecimal getCapi2BFsp() {
		return capi2BFsp;
	}

	public void setCapi2BFsp(BigDecimal capi2bFsp) {
		capi2BFsp = capi2bFsp;
	}

	public BigDecimal getCapi2BOutlier() {
		return capi2BOutlier;
	}

	public void setCapi2BOutlier(BigDecimal capi2bOutlier) {
		capi2BOutlier = capi2bOutlier;
	}

	public BigDecimal getNewTechPayAddOn() {
		return newTechPayAddOn;
	}

	public void setNewTechPayAddOn(BigDecimal newTechPayAddOn) {
		this.newTechPayAddOn = newTechPayAddOn;
	}

	public BigDecimal getLowVolPayment() {
		return lowVolPayment;
	}

	public void setLowVolPayment(BigDecimal lowVolPayment) {
		this.lowVolPayment = lowVolPayment;
	}

	public String getValBasedPurchPartipnt() {
		return valBasedPurchPartipnt;
	}

	public void setValBasedPurchPartipnt(String valBasedPurchPartipnt) {
		this.valBasedPurchPartipnt = valBasedPurchPartipnt;
	}

	public BigDecimal getValBasedPurchAdjust() {
		return valBasedPurchAdjust;
	}

	public void setValBasedPurchAdjust(BigDecimal valBasedPurchAdjust) {
		this.valBasedPurchAdjust = valBasedPurchAdjust;
	}

	public String getHospReadmissionRedu() {
		return hospReadmissionRedu;
	}

	public void setHospReadmissionRedu(String hospReadmissionRedu) {
		this.hospReadmissionRedu = hospReadmissionRedu;
	}

	public BigDecimal getHospHrrAdjustmt() {
		return hospHrrAdjustmt;
	}

	public void setHospHrrAdjustmt(BigDecimal hospHrrAdjustmt) {
		this.hospHrrAdjustmt = hospHrrAdjustmt;
	}

	public BigDecimal getOperBaseDrgPay() {
		return operBaseDrgPay;
	}

	public void setOperBaseDrgPay(BigDecimal operBaseDrgPay) {
		this.operBaseDrgPay = operBaseDrgPay;
	}

	public BigDecimal getOperDsh() {
		return operDsh;
	}

	public void setOperDsh(BigDecimal operDsh) {
		this.operDsh = operDsh;
	}

	public BigDecimal getCapiDsh() {
		return capiDsh;
	}

	public void setCapiDsh(BigDecimal capiDsh) {
		this.capiDsh = capiDsh;
	}

	public BigDecimal getCapiHspPct() {
		return capiHspPct;
	}

	public void setCapiHspPct(BigDecimal capiHspPct) {
		this.capiHspPct = capiHspPct;
	}

	public BigDecimal getCapiFspPct() {
		return capiFspPct;
	}

	public void setCapiFspPct(BigDecimal capiFspPct) {
		this.capiFspPct = capiFspPct;
	}

	public BigDecimal getArithAlos() {
		return arithAlos;
	}

	public void setArithAlos(BigDecimal arithAlos) {
		this.arithAlos = arithAlos;
	}

	public BigDecimal getPrWageIndex() {
		return prWageIndex;
	}

	public void setPrWageIndex(BigDecimal prWageIndex) {
		this.prWageIndex = prWageIndex;
	}

	public BigDecimal getTransferAdj() {
		return transferAdj;
	}

	public void setTransferAdj(BigDecimal transferAdj) {
		this.transferAdj = transferAdj;
	}

	public BigDecimal getUncompCareAmount() {
		return uncompCareAmount;
	}

	public void setUncompCareAmount(BigDecimal uncompCareAmount) {
		this.uncompCareAmount = uncompCareAmount;
	}

	public BigDecimal getBundleAdjustAmt() {
		return bundleAdjustAmt;
	}

	public void setBundleAdjustAmt(BigDecimal bundleAdjustAmt) {
		this.bundleAdjustAmt = bundleAdjustAmt;
	}

	public BigDecimal getValBasedPurchAdjustAmt() {
		return valBasedPurchAdjustAmt;
	}

	public void setValBasedPurchAdjustAmt(BigDecimal valBasedPurchAdjustAmt) {
		this.valBasedPurchAdjustAmt = valBasedPurchAdjustAmt;
	}

	public BigDecimal getReadmisAdjustAmt() {
		return readmisAdjustAmt;
	}

	public void setReadmisAdjustAmt(BigDecimal readmisAdjustAmt) {
		this.readmisAdjustAmt = readmisAdjustAmt;
	}

	public BigDecimal getEhrAdjustAmt() {
		return ehrAdjustAmt;
	}

	public void setEhrAdjustAmt(BigDecimal ehrAdjustAmt) {
		this.ehrAdjustAmt = ehrAdjustAmt;
	}
}
