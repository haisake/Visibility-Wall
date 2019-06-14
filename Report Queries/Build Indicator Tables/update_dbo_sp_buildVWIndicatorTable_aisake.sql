USE DSSI
GO

ALTER PROCEDURE [dbo].[sp_buildVWIndicatorTable_aisake]
AS
BEGIN

/*
Purpose: To create a consolidated query that constructs the indciators for the visibility wall
Author: Hans Aisake
Date Created: June 14, 2018
Date Modified: Jan 11, 2019
Inclusions/Exclusions:
Comments:

*/ 

------------------------
--Indentify true inpatient units
------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#adtcNUClassification_VW') IS NOT NULL DROP TABLE #adtcNUClassification_VW
	END

	BEGIN
		select f.[FacilityCode]
		, f.[FacilityShortName]
		, f.[FacilityLongName]
		, f.[Site]
		, n.NursingUnit as NursingUnitCode
		, NULevel = 'Acute'
		into #adtcNUClassification_VW
		FROM [ADTCMart].[dim].[NursingUnit] n
		left outer join [ADTCMart].[dim].[Facility] f 
		ON n.facilityID = f.facilityID
		WHERE f.[FacilityCode] in ('0001', '0002', '0007', '0112', 'PR', 'SM', 'SG', 'SPH', 'MSJ')
	END

	BEGIN
		ALTER TABLE #adtcNUClassification_VW
		ALTER COLUMN NULevel varchar(50) not null
	END

	BEGIN
		Update #adtcNUClassification_VW
		set NULevel = 'Extended Care'
		WHERE FacilityShortName in ('VGH', 'UBCH', 'RHS', 'LGH', 'MSJ', 'SGH')
		and NursingUnitCode in ('BP2E', 'BP2W', 'BP3E', 'BP3W', 'BP4E', 'BP4W', 'UP1E', 'UP1W', 'UP2E', 'UP2W', 'UP3E', 'UP3W', 'UP4E', 'UP4W'
		, 'M1E', 'M1W', 'M2E', 'M2W', 'M3W', 'EN1', 'EN2', 'ES1', 'ES2', 'ES3', 'M2W', 'MEC2','MEC3', 'H1N', 'H1S', 'H2N', 'H2S', 'HSU', 'HTN'
		,'NSH SSH')	--some could argue this isn't extended care but rather a stepdown unit
	
		Update #adtcNUClassification_VW
		set NULevel ='Hospice'
		WHERE FacilityShortName in ('VGH', 'UBCH', 'RHS', 'LGH', 'MSJ', 'SGH')
		AND NursingunitCode in ('NSH', 'PSJH')

		Update #adtcNUClassification_VW
		set NULevel = 'Day Care'
		WHERE FacilityShortName in ('VGH', 'RHS', 'LGH', 'PRGH', 'SPH', 'MSJ')
		and NursingUnitCode in ('MDC', 'RSDC', 'DCM', 'DCP', 'DCR', 'DCS', 'UP1E', 'PDC/INPT', 'SDC', 'MSDC')

		Update #adtcNUClassification_VW
		set NULevel = 'Transitional Care'
		WHERE FacilityShortName in ('UBCH', 'LGH')
		and NursingUnitCode in ('UK1T', 'A2T', 'TCU', 'DCP', 'DCR', 'DCS', 'UP1E', 'PDC/INPT', 'SDC')

		Update #adtcNUClassification_VW
		set NULevel = 'Tertiary MH'
		WHERE FacilityShortName in ('UBCH', 'VGH')
		and NursingUnitCode in ('WCC2', 'WP2', 'WP3', 'WP4', 'WP5', 'WP6', 'UD2S', 'UD2T')

		Update #adtcNUClassification_VW
		set NULevel = 'Geriatric Care'
		WHERE FacilityShortName in ('VGH')
		and NursingUnitCode in ('C5A','L5A')
	END

-----------------------------------------------
--Reporting TimeFrames
-----------------------------------------------
	BEGIN
		--weekly time frames, based on 1 day lag
		IF OBJECT_ID('tempdb.dbo.#VW_weekReportTF') IS NOT NULL DROP TABLE #VW_weekReportTF
	END
	
	--4 years	
	BEGIN
		SELECT distinct TOP 212  DATEADD(day, -6, ShortDate) as 'ThursdayWkStart', Shortdate as 'ThursdayWkEnd'
		INTO #VW_weekReportTF
		FROM ADTCMart.dim.[Date]
		WHERE shortDate <= DATEADD(day, -1, GETDATE())
		AND [DayOfWeek]='Thursday'
		ORDER BY shortdate DESC
	END

	--reporting periods, based on 7 day lag
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_FPReportTF') IS NOT NULL DROP TABLE #VW_FPReportTF
	END

	BEGIN
		SELECT distinct TOP 52 FiscalPeriodLong, fiscalperiodstartdate, fiscalperiodenddate
		INTO #VW_FPReportTF
		FROM ADTCMart.dim.[Date]
		WHERE fiscalperiodenddate <= DATEADD(day, -7, GETDATE())
		ORDER BY FiscalPeriodEndDate DESC
	END


	BEGIN
		--reporting quarters, based on 7 day lag
		IF OBJECT_ID('tempdb.dbo.#VW_QuarterReportTF') IS NOT NULL DROP TABLE #VW_QuarterReportTF
	END
	
	BEGIN
		SELECT distinct TOP 52 CAST(CalendarYear as char(4)) +'-'+CalendarQuarter as 'CalendarQuarter', CalendarQuarterStartDate, CalendarQuarterEndDate
		INTO #VW_QuarterReportTF
		FROM ADTCMart.dim.[Date]
		WHERE CalendarQuarterEndDate BETWEEN '2006-12-31' AND DATEADD(day, 31, GETDATE())	--have most of the quarter
		ORDER BY CalendarQuarterEndDate DESC
	END

----------------------------------
----placeholder
---------------------------------
	--to generate a place holder table to populate 0s for idnicators with scarce values
	
	BEGIN
		--identify the first value for each indicator and program combination
		IF OBJECT_ID('tempdb.dbo.#cutoffEarly') IS NOT NULL DROP TABLE #cutoffEarly
	END

	BEGIN
		SELECT Facility,IndicatorName, entity_group, MIN(TimeFrame) as 'FirstDate'
		INTO #cutoffEarly
		FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
		GROUP BY   Facility,IndicatorName, entity_group
	END

	BEGIN
	--identify all possible indicator week combinations
		IF OBJECT_ID('tempdb.dbo.#placeholder') IS NOT NULL DROP TABLE #placeholder
	END

	BEGIN
		SELECT * 
		INTO #placeholder
		FROM 
		(SELECT distinct ThursdaywkEnd as 'TimeFrame' FROM #VW_weekReportTF) as X
		CROSS JOIN 
		(	SELECT distinct  Facility, IndicatorName, Entity_Group 
			FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
			WHERE IndicatorName in ('# Direct Discharges from ED','Inpatient Census','Days >30 for Discharged Patients','Current Inpatient Long Length of Stay (LLOS) Inpatient Days*','Average ALC (all types) Census')
		) as Y
	END

	BEGIN
		--remove indicator rows that are too early
		DELETE X
		FROM #placeholder  as X 
		INNER JOIN #cutoffearly as Y
		ON  X.facility=Y.facility
		AND X.IndicatorName=Y.IndicatorName
		AND X.entity_group=Y.Entity_Group
		AND X.TimeFrame  <Y.FirstDate
	END


-----------------------------------------------
--ID 01 Percent of ED patients admitted to hospital within 10 hours - P4P
-----------------------------------------------
	/*
	Purpose: To compute how many people are admited into hospital FROM ED within 10 hrs of the decision to admit them.
	Author: Hans Aisake
	Date Created: June 14, 2018
	Date Updated: Jan 11, 2019
	Inclusions/Exclusions:
	Comments:
	*/

	BEGIN
		--preprocess ED data and identify reporting time frames
		IF OBJECT_ID('tempdb.dbo.#VW_ed01') IS NOT NULL DROP TABLE #VW_ed01
	END

	BEGIN
		--I wrote the computations in the complex way in an attempt to save a few seconds of computation; I am not sure I succeeded.
		SELECT 	T.ThursdayWkEnd
		, X.EDP_AdmitWithinTarget
		, X.Program
		, X.FacilityLongName
		INTO #VW_ed01
		FROM
		(
			SELECT ED.StartDate
			, ED.EDP_AdmitWithinTarget
			, ISNULL(MAP.NewProgram,'Unknown') as 'Program'
			, ED.FacilityLongName
			FROM EDMart.dbo.vwEDVisitIdentifiedRegional as ED
			LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP	--link to a fact table that identifies which program each unit goes to; not maintained by DMR
			ON ED.InpatientNursingUnitID= MAP.NursingUnitID			--same nursing unit id
			AND ED.StartDate BETWEEN MAP.StartDate AND MAP.EndDate	--within mapping dates; you could argue for inpatient date, but it's a minor issue
			WHERE ED.FacilityShortName='RHS'
			AND ED.admittedflag=1
			AND ED.StartDate >= (SELECT MIN(ThursdayWkStart) FROM #VW_weekReportTF)
		) as X
		INNER JOIN #VW_weekReportTF as T						--only keep reporting weeks we care about
		ON X.startdate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd

	END

	-----------------------------------------------
	--generate weekly indicators and store the data
	-----------------------------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID01') IS NOT NULL DROP TABLE #VW_ID01
	END

		BEGIN
		SELECT 	1 as 'IndicatorID'
		, FacilityLongName as 'Facility'
		, Program as 'Program'
		, thursdaywkend as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, 'Percent of ED Patients Admitted to Hospital Within 10 Hours' as 'IndicatorName'
		, sum(EDP_AdmitWithinTarget) as 'Numerator'
		, count(*) as 'Denominator'
		, 1.0*sum(EDP_AdmitWithinTarget)/count(*) as 'Value'
		, 'Above' as 'DesiredDirection'
		, 'P0' as 'Format'
		,  0.55 as 'Target'
		, 'EDMart' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID01
		FROM #VW_ed01
		GROUP BY thursdaywkend
		,Program
		, FacilityLongName
		--add overall indicator
		UNION
		SELECT 	1 as 'IndicatorID'
		, FacilityLongName as 'Facility'
		, 'Overall' as 'Program'
		, thursdaywkend as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, 'Percent of ED Patients Admitted to Hospital Within 10 Hours' as 'IndicatorName'
		, sum(EDP_AdmitWithinTarget) as 'Numerator'
		, count(*) as 'Denominator'
		, 1.0*sum(EDP_AdmitWithinTarget)/count(*)  as 'Value'
		, 'Above' as 'DesiredDirection'
		, 'P0' as 'Format'
		,  0.55 as 'Target'
		, 'EDMart' as 'DataSource'
		, 1 as 'IsOverall'
		FROM #VW_ed01
		GROUP BY thursdaywkend
		,FacilityLongName
	END

-----------------------------------------------
--ID 02 Short Stay discahrges (<=48hrs) excludes Newborns
-----------------------------------------------
	/*
	Purpose: To compute how many people are discahrged within 48hrs of admission
	Author: Hans Aisake
	Date Created: June 14, 2018
	Date Updated: June 14, 2018
	Inclusions/Exclusions:
		- true inpatient records only
		- excludes newborns
	Comments:
	*/

BEGIN
	IF OBJECT_ID('tempdb.dbo.#VW_SSad_02') IS NOT NULL DROP TABLE #VW_SSad_02
END

BEGIN
	SELECT T.ThursdayWkEnd
	,CASE WHEN datediff(mi,ADTC.[AdjustedAdmissionDate]+ADTC.[AdjustedAdmissionTime],ADTC.[AdjustedDischargeDate]+ADTC.[AdjustedDischargeTime]) between 0 and 1440 THEN '<=24 hrs'
			WHEN datediff(mi,ADTC.[AdjustedAdmissionDate]+ADTC.[AdjustedAdmissionTime],ADTC.[AdjustedDischargeDate]+ADTC.[AdjustedDischargeTime]) between 1441 and 2160 THEN '24 to 36 hrs'
			WHEN datediff(mi,ADTC.[AdjustedAdmissionDate]+ADTC.[AdjustedAdmissionTime],ADTC.[AdjustedDischargeDate]+ADTC.[AdjustedDischargeTime]) between 2161 and 2880 THEN '36 to 48 hrs'
			ELSE '>48 hrs' 
	END as 'LOSRange'
	,ISNULL(MAP.NewProgram,'Unknown') as 'Program'
	,1 as casecount
	INTO #VW_SSad_02
	FROM ADTCMart.[ADTC].[vwAdmissionDischargeFact] as ADTC
	INNER JOIN #VW_weekReportTF as T				
	on ADTC.[AdjustedDischargeDate] BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd	--only discharges in reporting time frame
	LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP		--a map of nursing unit to program; not maintained by dmr
	ON ADTC.DischargeNursingUnitCode=MAP.NursingUnitCode		--same code
	AND ADTC.AdjustedDischargeDate BETWEEN MAP.StartDate AND MAP.EndDate	--within the active dates
	WHERE [DischargeFacilityLongName]='Richmond Hospital'	--only richmond
	AND [AdjustedDischargeDate] is not null		--discharges only
	AND [DischargePatientServiceCode]<>'NB'		--exclude newborns
	AND [AccountType]='I'						--inpatient cases only
	AND [AdmissionAccountSubType]='Acute'		--inpatient subtype
END

BEGIN
	--compute and store metric
	IF OBJECT_ID('tempdb.dbo.#VW_ID02') IS NOT NULL DROP TABLE #VW_ID02
END

BEGIN
	SELECT 	2 as 'IndicatorID'
	, 'RHS' as 'Facility'
	, Program as 'Program'
	, thursdaywkend as 'TimeFrame'
	, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, 'Short Stay Discharges (LOS<=48hrs)' as 'IndicatorName'
	,sum(CASE WHEN losrange<>'>48 hrs' THEN 1 else 0 end) as 'Numerator'
	,count(*) as 'Denominator'
	, 1.0*sum(CASE WHEN losrange<>'>48 hrs' THEN 1 else 0 end)/count(*) as 'Value'
	, 'Below' as 'DesiredDirection'
	, 'P0' as 'Format'
	,  0.25 as 'Target'
	, 'ADTCMart' as 'DataSource'
	, 0 as 'IsOverall'
	INTO #VW_ID02
	FROM #VW_SSad_02
	GROUP BY thursdaywkend
	,Program
	--add overall
	UNION
	SELECT 	2 as 'IndicatorID'
	, 'RHS' as 'Facility'
	, 'Overall' as 'Program'
	, thursdaywkend as 'TimeFrame'
	, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, 'Short Stay Discharges (LOS<=48hrs)' as 'IndicatorName'
	, sum(case WHEN losrange<>'>48 hrs' THEN 1 else 0 end) as 'Numerator'
	, count(*) as 'Denominator'
	, 1.0*sum(case WHEN losrange<>'>48 hrs' THEN 1 else 0 end)/count(*) as 'Value'
	, 'Below' as 'DesiredDirection'
	, 'P0' as 'Format'
	,  0.25 as 'Target'
	, 'ADTCMart' as 'DataSource'
	, 1 as 'IsOverall'
	FROM #VW_SSad_02
	GROUP BY thursdaywkend
END

-----------------------------------------------
-- ID03 ALC rate Discharge Based Excluding NewBorns
-----------------------------------------------
	/*
	Purpose: To compute how many inpatient days were ALC vs. all inpatient days for patients discharged in the reporting time frame.

	# ALC inpatient days of discharges in time frame / # inpatient days of discharges in time frame (ALC and non-ALC)

	Author: Hans Aisake
	Date Created: June 14, 2018
	Date Updated: Oct 19, 2018
	Inclusions/Exclusions:
		- true inpatient records only
		- excludes newborns
	Comments:
	*/

	--links census data which has ALC information with admission/discharge information
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_discharges_03') IS NOT NULL DROP TABLE #VW_discharges_03
	END

	BEGIN
		SELECT AccountNumber
		, T.ThursdayWkEnd
		, ISNULL(MAP.NewProgram,'Unknown') as 'Program'
		, A.DischargeFacilityLongName
		, A.[site]
		INTO #VW_discharges_03
		FROM ADTCMart.[ADTC].[vwAdmissionDischargeFact] as A
		INNER JOIN #VW_weekReportTF as T						--identify the week
		ON A.AdjustedDischargeDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd
		LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP	--identify the program
		ON A.DischargeNursingUnitCode = MAP.nursingunitcode
		AND A.AdjustedDischargeDate BETWEEN MAP.StartDate AND MAP.EndDate
		WHERE A.[Site]='rmd'
		AND A.[AdjustedDischargeDate] is not null		--discharged
		AND A.[DischargePatientServiceCode]<>'NB'		--not a new born
		AND A.[AccountType]='I'							--inpatient at richmond only
		AND A.[AdmissionAccountSubType]='Acute'			--subtype acute; true inpatient
		AND LEFT(A.DischargeNursingUnitCode,1)!='M'	--excludes ('Minoru Main Floor East','Minoru Main Floor West','Minoru Second Floor East','Minoru Second Floor West','Minoru Third Floor')
		AND A.AdjustedDischargeDate > (SELECT MIN(thursdaywkstart) FROM #VW_weekReportTF)	--discahrges in reporting timeframe
	END

	BEGIN
		--links census data which has ALC information with admission/discharge information
		IF OBJECT_ID('tempdb.dbo.#VW_ALC_discharges_03') IS NOT NULL DROP TABLE #VW_ALC_discharges_03
	END

	--pull in ALC days per case
	BEGIN
		SELECT C.AccountNum
		, SUM(CASE WHEN patientservicecode like 'AL[0-9]' or patientservicecode like 'A1[0-9]' THEN 1 ELSE 0 END) as 'ALC_Days'
		, COUNT(*) as 'Census_Days'
		INTO #VW_ALC_discharges_03
		FROM ADTCMart.adtc.vwCensusFact as C
		WHERE exists (SELECT 1 FROM #VW_discharges_03 as Y WHERE C.AccountNum=Y.AccountNumber AND C.[Site]=Y.[Site])
		GROUP BY C.AccountNum
	END
	

	BEGIN
		--compute and store metric
		IF OBJECT_ID('tempdb.dbo.#VW_ID03') IS NOT NULL DROP TABLE #VW_ID03
	END

	BEGIN
		SELECT 	3 as 'IndicatorID'
		, X.DischargeFacilityLongName as 'Facility'
		, X.Program as 'Program'
		, X.thursdaywkend as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,X.ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, 'ALC Rate Based on Discharges' as 'IndicatorName'
		, SUM(Y.ALC_Days) as 'Numerator'
		, SUM(Y.Census_Days) as 'Denominator'
		, 1.0*SUM(Y.ALC_Days)/SUM(Y.Census_Days) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'P0' as 'Format'
		, CASE WHEN X.ThursdayWkEnd between '4/1/2013' and '3/31/2014' THEN 0.099
			  WHEN X.ThursdayWkEnd between '4/1/2014' and '3/31/2015' THEN 0.11
			  WHEN X.ThursdayWkEnd between '4/1/2015' and '3/31/2016' THEN 0.115
			  ELSE 0.115 
		END as 'Target'
		, 'ADTCMart' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID03
		FROM #VW_discharges_03 as X
		LEFT JOIN #VW_ALC_discharges_03 as Y
		ON X.AccountNumber=Y.AccountNum
		GROUP BY X.thursdaywkend
		, X.Program
		,CASE WHEN X.ThursdayWkEnd between '4/1/2013' and '3/31/2014' THEN 0.099
			  WHEN X.ThursdayWkEnd between '4/1/2014' and '3/31/2015' THEN 0.11
			  WHEN X.ThursdayWkEnd between '4/1/2015' and '3/31/2016' THEN 0.115
			  ELSE 0.115 
		END
		, X.DischargeFacilityLongName
		--add overall
		UNION 
		SELECT 	3 as 'IndicatorID'
		, X.DischargeFacilityLongName as 'Facility'
		, 'Overall' as 'Program'
		, X.thursdaywkend as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,X.ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, 'ALC Rate Based on Discharges' as 'IndicatorName'
		, SUM(Y.ALC_Days) as 'Numerator'
		, SUM(Y.Census_Days) as 'Denominator'
		, 1.0*SUM(Y.ALC_Days)/SUM(Y.Census_Days) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'P0' as 'Format'
		, CASE WHEN X.ThursdayWkEnd between '4/1/2013' and '3/31/2014' THEN 0.099
			  WHEN X.ThursdayWkEnd between '4/1/2014' and '3/31/2015' THEN 0.11
			  WHEN X.ThursdayWkEnd between '4/1/2015' and '3/31/2016' THEN 0.115
			  ELSE 0.115 
		END as 'Target'
		, 'ADTCMart' as 'DataSource'
		, 1 as 'IsOverall'
		FROM #VW_discharges_03 as X
		LEFT JOIN #VW_ALC_discharges_03 as Y
		ON X.AccountNumber=Y.AccountNum
		GROUP BY X.thursdaywkend
		,CASE WHEN X.ThursdayWkEnd between '4/1/2013' and '3/31/2014' THEN 0.099
			  WHEN X.ThursdayWkEnd between '4/1/2014' and '3/31/2015' THEN 0.11
			  WHEN X.ThursdayWkEnd between '4/1/2015' and '3/31/2016' THEN 0.115
			  ELSE 0.115 
		END
		, X.DischargeFacilityLongName
	END

-----------------------------------------------
-- ID04 Average ALC Census excludes newborns
-----------------------------------------------
	/*
	Purpose: To compute average ALC census excluding newborns for patient currently in hospital.
	This doesn't consider discharge date.

	# ALC days (clients?) in the time frame / # of days in the time frame

	Author: Hans Aisake
	Date Created: June 14, 2018
	Date Updated: October 19, 2018
	Inclusions/Exclusions:
		- true inpatient records only
		- excludes newborns
	Comments:
		*******This isn't quite right for the acutal census. It's something like alc days per day but not quite.
	*/

BEGIN
	IF OBJECT_ID('tempdb.dbo.#VW_ALCcensus_04') IS NOT NULL DROP TABLE #VW_ALCcensus_04
END

BEGIN
	SELECT C.censusdate
	, C.accountnum
	, ISNULL(MAP.NewProgram,'Unknown') as 'Program'
	, C.FacilityLongName
	INTO #VW_ALCcensus_04
	FROM ADTCMart.[ADTC].[vwCensusFact] as C
	LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP			--get the program of the census nursing unit
	ON C.NursingUnitCode=MAP.nursingunitcode						--same unit code
	AND C.CensusDate BETWEEN MAP.StartDate AND MAP.EndDate			--unit-program active mapping dates
	WHERE C.[Site]='rmd'				--richmond only
	AND LEFT(C.nursingUnitCode,1) !='M' --excludes ('Minoru Main Floor East','Minoru Main Floor West','Minoru Second Floor East','Minoru Second Floor West','Minoru Third Floor')
	AND C.[PatientServiceCode]<>'NB'	--excludes new borns
	AND C.[AccountType]='Inpatient'		--inpatients only
	AND C.[AccountSubType]='Acute'		--true inpatients; ingnores extended and strange data errors
	AND (C.patientservicecode like 'AL[0-9]' or C.patientservicecode like 'A1[0-9]')	--only ALC days
	AND C.CensusDate > (SELECT MIN(thursdaywkstart) FROM #VW_weekReportTF)				--census in reporting timeframe
END

--compute and store metric
	
BEGIN
	IF OBJECT_ID('tempdb.dbo.#VW_ID04') IS NOT NULL DROP TABLE #VW_ID04
END

BEGIN
	SELECT 	4 as 'IndicatorID'
	, C.FacilityLongName as 'Facility'
	, C.Program as 'Program'
	, T.thursdaywkend as 'TimeFrame'
	, CONVERT(VARCHAR, CONVERT(date,T.ThursdayWkEnd), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, 'Average ALC (all types) Census' as 'IndicatorName' 
	, NULL as 'Numerator'
	, NULL as 'Denominator'
	, SUM(1)/7.0 as 'Value'
	, 'Below' as 'DesiredDirection'
	, 'D1' as 'Format'
	, NULL as 'Target'
	, 'ADTCMart' as 'DataSource'
	, 0 as 'IsOverall'
	INTO #VW_ID04
	FROM #VW_ALCcensus_04 as C
	INNER JOIN #VW_weekReportTF as T
	ON C.CensusDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd
	GROUP BY T.thursdaywkend
	, C.Program
	, C.[FacilityLongName]
	--add overall
	UNION
	SELECT 	4 as 'IndicatorID'
	, C.[FacilityLongName] as 'Facility'
	, 'Overall' as 'Program'
	, T.thursdaywkend as 'TimeFrame'
	, CONVERT(VARCHAR, CONVERT(date,T.ThursdayWkEnd), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, 'Average ALC (all types) Census' as 'IndicatorName' 
	, NULL as 'Numerator'
	, NULL as 'Denominator'
	, SUM(1)/7.0 as 'Value'
	, 'Below' as 'DesiredDirection'
	, 'D1' as 'Format'
	, 23 as 'Target'
	, 'ADTCMart' as 'DataSource'
	, 1 as 'IsOverall'
	FROM #VW_ALCcensus_04 as C
	INNER JOIN #VW_weekReportTF as T
	ON C.CensusDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd
	GROUP BY T.thursdaywkend
	, C.[FacilityLongName]
END

--append 0's
BEGIN
	INSERT INTO #VW_ID04 (IndicatorID, Facility,IndicatorName, Program,TimeFrame, TimeFrameLabel, TimeFrameType,Numerator,Denominator,[Value],DesiredDirection,[Format],[Target],DataSource,IsOverall)
	SELECT 	4 as 'IndicatorID'
	, p.facility
	, p.IndicatorName
	, P.entity_group
	, P.TimeFrame
	, CONVERT(VARCHAR, CONVERT(date,P.TimeFrame), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, NULL as 'Numerator'
	, NULL as 'Denominator'
	, 0 as 'Value'	--proper 0's
	, 'Below' as 'DesiredDirection'
	, 'D1' as 'Format'
	, NULL as 'Target'
	, 'ADTCMart' as 'DataSource'
	, CASE WHEN P.entity_group ='Overall' THEN 1 ELSE 0 END as 'IsOverall'
	FROM #placeholder as P
	LEFT JOIN #VW_ID04 as I
	ON P.facility=I.Facility
	AND P.IndicatorName=I.IndicatorName
	AND P.entity_group=I.Program
	AND P.TimeFrame=I.TimeFrame
	WHERE P.IndicatorName = (SELECT distinct IndicatorName FROM #VW_ID04)
	AND I.[Value] is NULL
END

-----------------------------------------------
-- ID05 Days > 30 for Discharge Patients excludes newborns
-----------------------------------------------
	/*
	Purpose: To compute the volume of discharges that were LLOS >30 days patients.
	Author: Hans Aisake
	Date Created: June 14, 2018
	Date Updated: Oct 19, 2018
	Inclusions/Exclusions:
		- true inpatient records only
		- excludes newborns
	Comments:
	*/

BEGIN
	IF OBJECT_ID('tempdb.dbo.#VW_LLOS_05') IS NOT NULL DROP TABLE #VW_LLOS_05
END

BEGIN
	SELECT T.ThursdayWkEnd
	, ADTC.LOSDays 
	, ISNULL(MAP.NewProgram,'Unknown') as 'Program'
	,1 as 'casecount'
	, ADTC.DischargeFacilityLongName
	INTO #VW_LLOS_05
	FROM ADTCMart.[ADTC].[vwAdmissionDischargeFact] as ADTC
	INNER JOIN #VW_weekReportTF as T
	ON ADTC.AdjustedDischargeDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd	--discharge in reporting timeframe
	LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP		--to get the program
	ON ADTC.DischargeNursingUnitCode=MAP.nursingunitcode		--same code
	AND ADTC.AdjustedDischargeDate BETWEEN MAP.StartDate AND MAP.EndDate	--unit program mapping dates
	WHERE ADTC.[site]='rmd'	--richmond only
	and ADTC.[AdjustedDischargeDate] is not null	--must have a discharge date
	and ADTC.[DischargePatientServiceCode]<>'NB'	--exclude newborns
	and ADTC.[AccountType]='I'						--inpatient cases only
	and ADTC.[AdmissionAccountSubType]='Acute'		--inpatient subtype
	and LEFT(ADTC.DischargeNursingUnitCode,1)!='M'	--excludes ('Minoru Main Floor East','Minoru Main Floor West','Minoru Second Floor East','Minoru Second Floor West','Minoru Third Floor')
	and ADTC.LOSDays>30
END

--compute and store metric
BEGIN
	IF OBJECT_ID('tempdb.dbo.#VW_ID05') IS NOT NULL DROP TABLE #VW_ID05
END

BEGIN
	SELECT 5 as 'IndicatorID'
	, DischargeFacilityLongName as 'Facility'
	, Program as 'Program'
	, thursdaywkend as 'TimeFrame'
	, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, 'Days >30 for Discharged Patients' as 'IndicatorName' 
	, NULL as 'Numerator'
	, NULL as 'Denominator'
	,sum(LOSDays-30) as 'Value'
	, 'Below' as 'DesiredDirection'
	, 'D0' as 'Format'
	, NULL as 'Target'
	, 'ADTCMart' as 'DataSource'
	, 0 as 'IsOverall'
	INTO #VW_ID05
	FROM #VW_LLOS_05
	GROUP BY thursdaywkend
	,Program
	,DischargeFacilityLongName
	--add overall
	UNION
	SELECT 5 as 'IndicatorID'
	, DischargeFacilityLongName as 'Facility'
	, 'Overall' as 'Program'
	, thursdaywkend as 'TimeFrame'
	, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, 'Days >30 for Discharged Patients' as 'IndicatorName' 
	, NULL as 'Numerator'
	, NULL as 'Denominator'
	, sum(LOSDays-30) as 'Value'
	, 'Below' as 'DesiredDirection'
	, 'D0' as 'Format'
	, 13210.0/365*7 as 'Target'
	, 'ADTCMart' as 'DataSource'
	, 1 as 'IsOverall'
	FROM #VW_LLOS_05
	GROUP BY thursdaywkend
	,DischargeFacilityLongName
END

	--append 0's
BEGIN
	INSERT INTO #VW_ID05 (IndicatorID, Facility,IndicatorName,Program,TimeFrame,TimeFrameLabel, TimeFrameType,Numerator,Denominator,[Value],DesiredDirection,[Format],[Target],DataSource,IsOverall)
	SELECT 	5 as 'IndicatorID'
	, p.facility
	, p.IndicatorName
	, P.entity_group
	, P.TimeFrame
	, CONVERT(VARCHAR, CONVERT(date,P.TimeFrame), 120) as 'TimeFrameLabel'
	, 'Weekly'
	, NULL as 'Numerator'
	, NULL as 'Denominator'
	, 0 as 'Value'	--proper 0's
	, 'Below' as 'DesiredDirection'
	, 'D0' as 'Format'
	, CASE WHEN entity_group='Overall' THEN 13210.0/365*7 ELSE NULL END  as 'Target'
	, 'ADTCMart' as 'DataSource'
	, CASE WHEN P.entity_group ='Overall' THEN 1 ELSE 0 END as 'IsOverall'
	FROM #placeholder as P
	LEFT JOIN #VW_ID05 as I
	ON P.facility=I.Facility
	AND P.IndicatorName=I.IndicatorName
	AND P.entity_group=I.Program
	AND P.TimeFrame=I.TimeFrame
	WHERE P.IndicatorName = (SELECT distinct IndicatorName FROM #VW_ID05)
	AND I.[Value] is NULL
END


-----------------------------------------------
-- ID06 Current Inpatient Days >30
-----------------------------------------------
	/*
	Purpose: To compute the LLOS census of currently waiting on the thursday week end of each week. (Snapshot)
	Fundamentally, It seams like we are trying our best to just focus on true inpatients in true inpatient units.
	It's not clear though what the expected definition is.

	The P4P query applies filters in a way that doesn't make this quite clear so I've switched it around here.

	Author: Hans Aisake
	Date Created: June 14, 2018
	Date Updated: June 14, 2018
	Inclusions/Exclusions:
		- true inpatient records only
		- excludes newborns
	Comments:
		I took the base query for indicator 471 for the BSC version FROM Emily, but modified it to be richmond specific. The numbers might not match perfectly.
		Targets are absed on the BSI fiscal period targets. If it is a snapshot it shouldn't be directly comprable to the thursday week end.
	
	*/
	-------------------
	--pull LLOS (>30 days) census data for patients 
	------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_cLLOS_Census') IS NOT NULL DROP TABLE #VW_cLLOS_Census
	END

	BEGIN
		SELECT  -- Identify number of days greater than 30
		CASE WHEN ADTC.AdmittoCensusDays > 210 THEN 180
			 ELSE ADTC.AdmittoCensusDays-30 
		END as 'LLOSDays'
		, ISNULL(MAP.NewProgram,'Unknown') as 'Program'
		, D.ThursdayWkEnd
		, ADTC.FacilityLongName
		, ADTC.PatientId
		INTO #VW_cLLOS_Census
		FROM ADTCMart.[ADTC].[vwCensusFact] as ADTC
		INNER JOIN #VW_weekReportTF as D 
		ON ADTC.CensusDate =D.ThursdayWkEnd	--pull census for the thursday weeks, as a snapshot
		INNER JOIN #adtcNUClassification_VW as NU
		ON ADTC.NursingUnitCode=NU.NursingUnitCode					--match on nursing unit
		AND NU.NULevel='Acute'										--only inpatient units
		LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP		--get program
		ON ADTC.NursingUnitCode=MAP.nursingunitcode					--match on nursing unit
		AND ADTC.CensusDate BETWEEN MAP.StartDate AND MAP.EndDate	--active program unit mapping dates
		WHERE ADTC.age>1				--P4P standard definition to exclude newborns.
		AND ADTC.AdmittoCensusDays > 30	--only need the LLOS patients, I'm not interested in proportion of all clients
		AND (ADTC.HealthAuthorityName = 'Vancouver Coastal' -- only include residents of Vancouver Coastal
		OR (ADTC.HealthAuthorityName = 'Unknown BC' AND (ADTC.IsHomeless = '1' OR ADTC.IsHomeless_PHC = '1'))) -- Include Unknown BC homeless population
		AND ADTC.[Site] ='rmd'									--only include census at Richmond
		AND ADTC.AccountType in ('I', 'Inpatient', '391')		--the code is different for each facility. Richmond is Inpatient
		AND ADTC.AccountSubtype in ('Acute')					--the true inpatient classification is different for each site. This is the best guess for Richmond
		--didn't have filters for patient service. that might be wrong on my part
	END

	--compute and store metric
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID06') IS NOT NULL DROP TABLE #VW_ID06
	END

	BEGIN
		SELECT 	6 as 'IndicatorID'
		, FacilityLongName as 'Facility'
		, Program as 'Program'
		, thursdaywkend as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, 'Current Inpatient Long Length of Stay (LLOS) Inpatient Days*' as 'IndicatorName' 
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, SUM(LLOSDays) as 'Value'
		 , 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		, NULL as 'Target'
		, 'ADTCMart' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID06
		FROM #VW_cLLOS_Census
		GROUP BY thursdaywkend
		,Program
		, FacilityLongName
		--add overall
		UNION
		SELECT 	6 as 'IndicatorID'
		, FacilityLongName as 'Facility'
		, 'Overall' as 'Program'
		, thursdaywkend as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, 'Current Inpatient Long Length of Stay (LLOS) Inpatient Days*' as 'IndicatorName' 
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, SUM(LLOSDays) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		, CASE WHEN ThursdayWkEnd BETWEEN '2012-04-01' AND '2013-03-31' THEN 1697
			   WHEN ThursdayWkEnd BETWEEN '2013-04-01' AND '2014-03-31' THEN 1697
			   WHEN ThursdayWkEnd BETWEEN '2014-04-01' AND '2015-03-31' THEN 1432
			   WHEN ThursdayWkEnd BETWEEN '2015-04-01' AND '2016-03-31' THEN 1454
			   WHEN ThursdayWkEnd BETWEEN '2016-04-01' AND '2017-03-31' THEN 1381
			   WHEN ThursdayWkEnd BETWEEN '2017-04-01' AND '2018-03-31' THEN 1376
			   ELSE NULL
		END as 'Target'
		, 'ADTCMart' as 'DataSource'
		, 1 as 'IsOverall'
		FROM #VW_cLLOS_Census
		GROUP BY thursdaywkend
		, FacilityLongName
	END

	BEGIN
		--append 0's ; might be something wrong here
		INSERT INTO #VW_ID06 (IndicatorId, Facility,IndicatorName,Program,TimeFrame,TimeFrameLabel,TimeFrameType,Numerator,Denominator,[Value],DesiredDirection,[Format],[Target],DataSource,IsOverall)
		SELECT 	6 as 'IndicatorID'
		, p.facility
		, p.IndicatorName
		, P.entity_group
		, P.TimeFrame
		, CONVERT(VARCHAR, CONVERT(date,P.TimeFrame), 120) as 'TimeFrameLabel'
		, 'Weekly'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, 0 as 'Value'	--proper 0's
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		, CASE WHEN entity_group='Overall' THEN 13210.0/365*7 ELSE NULL END  as 'Target'
		, 'ADTCMart' as 'DataSource'
		, CASE WHEN P.entity_group ='Overall' THEN 1 ELSE 0 END as 'IsOverall'
		FROM #placeholder as P
		LEFT JOIN #VW_ID06 as I
		ON P.facility=I.Facility
		AND P.IndicatorName=I.IndicatorName
		AND P.entity_group=I.Program
		AND P.TimeFrame=I.TimeFrame
		WHERE P.IndicatorName = (SELECT distinct IndicatorName FROM #VW_ID06)
		AND I.[Value] is NULL
	END

-----------------------------------------------
-- ID07 Ambulatory Visits as a % of Total Home Nursing Visits
-----------------------------------------------
	/*
	Purpose: % of Face to Face HN Visits that were Ambulatory.
	Using CommunityMart Casenotes instead of the finance numbers.
	The old query was referencing BSI, I've switch to the HH dashboard query here.
	Author: Ken Hawkins
	Co-Author: Hans Aisake
	Date Created: July 13, 2017
	Date Modified: Oct 19, 2018
	Inclusions/exclsusions: 
	Comments: 
	Changed the location logic from a proxy based on PARIS team to use ContactSetting ='Ambulatory Home Care' from the casenotes.
	Ken identified this method as having more accurate location data than the old method of basing it on PARIS team.

	*/

--get data from PARIS
BEGIN
	IF OBJECT_ID('tempdb.dbo.#tempAmb_VW') IS NOT NULL DROP TABLE #tempAmb_VW
END

BEGIN
	SELECT CC.[CommunityRegion]
	, CC.CommunityProgramGroup
	, D.ThursdayWkEnd
	, COUNT(distinct case when CC.contactsetting ='Ambulatory Home Care' THEN CC.[CaseNoteContactServiceKey] ELSE NULL END ) as 'NumAmbVisits'	--change this definition from PARIS team names to contactsetting=
	, COUNT(distinct CC.[CaseNoteContactServiceKey]) as 'NumTotalVisits'
	INTO #tempAmb_VW
	FROM [CommunityMart].[dbo].[vwPARISCaseNoteContact] as CC	--case note information with contact details
	INNER JOIN (SELECT SourceCaseNoteHeaderID, CaseNoteDate FROM CommunityMart.dbo.vwPARISCaseNoteHeader) as CH		--there is a requqest to get a date field into the view.
	ON CC.SourceCaseNoteHeaderID =CH.SourceCaseNoteHeaderID
	INNER JOIN #VW_weekReportTF as D
	ON CH.CaseNoteDate BETWEEN D.ThursdayWkStart AND D.ThursdayWkEnd		--case note durring the week.
	WHERE CC.[ReferralReasonServiceGroup] = 'Nursing'				--service filter
	AND CC.[CommunityProgramGroup] = 'Home & Community Care'		--not sure how the remove RC/AL/Hospice plays in if at all for this indicator. At present those clients are included.
	AND CC.[CaseNoteContactType] = 'Face To Face'		--face to face visit's only for this indicator
	AND CC.Communityregion='Richmond'		--richmond only
	GROUP BY CC.[CommunityRegion]
	, CC.CommunityProgramGroup
	, D.ThursdayWkEnd
END

--store results
BEGIN
	IF OBJECT_ID('tempdb.dbo.#VW_ID07') IS NOT NULL DROP TABLE #VW_ID07
END

BEGIN
	SELECT 7 as 'IndicatorID'
	, 'Richmond Community' as 'Facility'
	, CommunityProgramGroup as 'Program'
	, thursdaywkend as 'TimeFrame'
	, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
	, 'Weekly' as 'TimeFrameType'
	, '% of Face-to-Face Nursing Visits that were Ambulatory' as 'IndicatorName'
	, SUM(NumAmbVisits) as 'Numerator'
	, SUM(NumTotalVisits) as 'Denominator'
	, 1.0*SUM(NumAmbVisits)/SUM(NumTotalVisits) as 'Value'
	, 'Above' as 'DesiredDirection'
	, 'P1' as 'Format'
	, 0.4 as 'Target'
	, 'CaseNotes' as 'DataSource'
	, 0 as 'IsOverall'
	INTO #VW_ID07
	FROM #tempAmb_VW
	GROUP BY thursdaywkend
	, CommunityProgramGroup
	--no overall, or breakdown depending on how you look at it
END

-----------------------------------------------
-- ID09 and ID10 RC priority Access Placements by Placed From Location type data prep
-----------------------------------------------
	/*
		Purpose: To compute the number of placements from location categories
		, as well as the percentage of cases placed within target wait times.
		Stores the data into a table for the SSRS report to pull from.
		Needs to be run for each update
		Author: Hans Aisake
		Date Created: November 24, 2017
		Date Modified: May 2, 2018
		Inclusions/Exclusions: 
		-Includes palcements from the Richmond Priority Access Wait List only.
		Comments:
		-wait time is Date on to Date off.
		-These wait times do include suspended time.
		-30 day acute and community targets were given by Alfonso and Natalie; but no one really knows what the targets should be.
		-none acute patients in inpatient units with account type inpatient will be considered acute. This means patients in TCU will be considered as placed from acute.
	*/

	-----------------------------------
	--		Pull WL Data
	-----------------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_rcdata') IS NOT NULL DROP TABLE #VW_rcdata
	END

	BEGIN
		SELECT D.CalendarQuarter
		, D.CalendarQuarterEndDate
		, W.WaitlistName
		, W.[SourceSystemClientID]
		, W.[DateOnWaitlist]
		, W.[DateOffWaitlist]
		, W.PatientID
		, W.Communityregion
		, IIF(P.DeathDate >W.DateOffWaitlist OR P.DeathDate is NULL, w.[SourceSystemClientID], NULL) as 'PlacedBeforeDeath_SourceSystemClientID'
		, W.WaitlistOfferOutcome
		, W.SourceWaitlistEntryID
		, MAP.custom_locationType
		, MAP.custom_locationCategory
		, ISNULL(MAP.WaitTimeTarget,30) as 'Target'
		INTO #VW_rcdata
		FROM CommunityMart.[dbo].[vwPARISWaitlist] as w
		LEFT JOIN CommunityMart.[dbo].[vwPARISPersonWithIdentifier] as p --get patient death date as a data quality control; exclude deaths from still waiting
		ON p.[PatientID]=w.[PatientID]							--same patient
		INNER JOIN #VW_QuarterReportTF as D					--get reporting time frame
		ON W.DateOffWaitlist BETWEEN D.CalendarQuarterStartDate AND D.CalendarQuarterEndDate
		LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_AF_RCLocationTypeMap] as MAP
		ON W.CurrentLocationType_DateOffWaitlist = MAP.cmart_CurrentLocationtype	--location type matches
		WHERE W.WaitlistName in ('PRIORITY ACCESS RICHMOND')	--priority access watlist for Richmond
		AND W.Dateoffwaitlist is not null						--placements or removals only
	END

	--moved this to a second step to improve performance
	BEGIN
		DELETE 
		FROM #VW_rcdata
		WHERE WaitlistOfferOutcome not like 'Client Placed%'
	END

	---------------------------------------
	--Pull suspended time
	---------------------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_susData') IS NOT NULL DROP TABLE #VW_susData
	END
	
	BEGIN
		SELECT SourceWaitListEntryID
		, SUM(DATEDIFF(day, StartDate, EndDate)) as 'SuspendedDays'
		INTO #VW_susData
		FROM (
			SELECT SourceWaitListEntryID
			, CONVERT(date, CONVERT(varchar(8),StartDateID), 112) as 'StartDate'
			, CONVERT(date, CONVERT(varchar(8),EndDateID), 112) as 'EndDate'
			FROM COmmunityMart.[dbo].[WaitlistSuspensionFact] as WSF
			WHERE exists (SELECT 1 FROM #VW_rcdata as D WHERE D.SourceWaitlistEntryID=WSF.SourceWaitListEntryID)
			AND IsInvalid=0
		) X
		GROUP BY SourceWaitListEntryID
	END

	---------------------------------------
	--pull adtc discharge data for true inpatient units for linking to the RC data for better location information
	---------------------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_adtc_09') IS NOT NULL DROP TABLE #VW_adtc_09
	END
	
	BEGIN
		SELECT PatientID, AdjustedAdmissionDate, AdjustedDischargeDate
		INTO #VW_adtc_09
		FROM ADTCMart.adtc.vwAdmissionDischargeFact as ADTC 
		WHERE AccountType in ('I', 'Inpatient', '391')	--inpatient records only, doesn't check subtypes.
		AND DischargeNursingUnitCode in (select distinct NursingUnitCode from #adtcNUClassification_VW where Nulevel = 'Acute')	
		AND exists (SELECT 1 FROM #VW_rcdata as H WHERE ADTC.PatientID=H.PatientID)
	END

	---------------------------------------
	--combine the CommunityMart Placement data with ADTC for the placed from location
	---------------------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_rc_adtcLinkedData') IS NOT NULL DROP TABLE #VW_rc_adtcLinkedData
	END
	
	BEGIN
		SELECT H.CalendarQuarter
		, H.CalendarQuarterEndDate
		, H.WaitlistName
		, H.SourceSystemClientID
		, H.PatientId
		, H.CommunityRegion
		, H.DateOnWaitlist
		, H.DateOffWaitlist
		, DATEDIFF(Day, H.DAteOnWaitlist, H.DateOffWaitlist) - ISNULL(SUS.SuspendedDays,0) as 'WaitTime'
		, H.[Target]
		, CASE	WHEN H.Custom_LocationType ='Acute' AND ADTC.PatientID is not NULL THEN 'Acute'
				WHEN H.Custom_LocationType ='Acute' AND ADTC.PatientID is NULL THEN 'UnknownCommunity'
				WHEN H.Custom_LocationType !='Acute' AND ADTC.PatientID is NULL THEN H.Custom_LocationType
				WHEN H.Custom_LocationType !='Acute' AND ADTC.PatientID is not NULL THEN 'Acute'
				ELSE 'Error'			
		END as 'PlacedFromLocation'
		, CASE	WHEN H.Custom_LocationCategory ='Acute' AND ADTC.PatientID is not NULL THEN 'Acute'
				WHEN H.Custom_LocationCategory ='Acute' AND ADTC.PatientID is NULL THEN 'Community'
				WHEN H.Custom_LocationCategory !='Acute' AND ADTC.PatientID is NULL THEN H.Custom_LocationCategory
				WHEN H.Custom_LocationCategory !='Acute' AND ADTC.PatientID is not NULL THEN 'Acute'
				ELSE 'Error'			
		END as 'PlacedFromLocationCategory'
		, H.SourceWaitlistEntryID
		INTO #VW_rc_adtcLinkedData
		FROM #VW_rcdata as H
		LEFT JOIN #VW_adtc_09 as ADTC
		ON H.DateOffWaitlist=ADTC.[AdjustedDischargeDate] 
		AND H.PatientID=ADTC.PatientId
		LEFT JOIN #VW_susData as SUS
		ON H.SourceWaitlistEntryID=SUS.SourceWaitlistEntryID
	END

	------------------------------
	--find number of placements and placements within target
	------------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#tempPlacements_VW') IS NOT NULL DROP TABLE #tempPlacements_VW
	END

	BEGIN
		SELECT CalendarQuarter
		, CalendarQuarterEndDate
		, PlacedFromLocation
		, PlacedFromLocationCategory
		, CASE	WHEN Waittime BETWEEN 0 AND 30 THEN '0-30 Days'
				WHEN Waittime BETWEEN 31 AND 60 THEN '31-60 Days'
				WHEN Waittime BETWEEN 61 AND 90 THEN '61-90 Days'
				WHEN Waittime >= 91 THEN '91+ Days'
		END as 'WaitTimeGroup'
		, COUNT(distinct PatientID) as 'NumPlacements'
		, COUNT(distinct CASE WHEN Waittime <= [Target] THEN PatientId ELSE NULL END) as 'NumPlacedWithinTarget'
		INTO #tempPlacements_VW
		FROM #VW_rc_adtcLinkedData
		WHERE waittime is not NULL AND waittime >=0			--have to exclude these strange errors
		GROUP BY CalendarQuarter
		, CalendarQuarterEndDate
		, PlacedFromLocation
		, PlacedFromLocationCategory
		, CASE	WHEN Waittime BETWEEN 0 AND 30 THEN '0-30 Days'
				WHEN Waittime BETWEEN 31 AND 60 THEN '31-60 Days'
				WHEN Waittime BETWEEN 61 AND 90 THEN '61-90 Days'
				WHEN Waittime >= 91 THEN '91+ Days'
		END
	END

	--store placement data into temporary table
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#tempPlacements_VW2') IS NOT NULL DROP TABLE #tempPlacements_VW2
	END

	BEGIN
		SELECT distinct CJ.*
		, CASE	WHEN CJ.PlacedFromLocation ='Acute' THEN 0.7
				ELSE 0.8
		END as 'Target'	--from QUIST report on November 24, 2017
		, ISNULL(P.NumPlacements,0) as 'NumPlacements'
		, ISNULL(P.NumPlacedWithinTarget,0) as 'NumPlacedWithinTarget'		
		, GETDATE() as 'UpdateDate'
		INTO #tempPlacements_VW2
		FROM
		(SELECT * FROM
		(SELECT distinct CalendarQuarter,CalendarQuarterEndDate FROM #VW_QuarterReportTF) as X
		CROSS JOIN
		(SELECT distinct PlacedFromLocation FROM #tempPlacements_VW) as Y
		CROSS JOIN
		(SELECT distinct PlacedFromLocationCategory FROM #tempPlacements_VW) as A
		CROSS JOIN
		(SELECT distinct WaitTimeGroup FROM #tempPlacements_VW) as Z
		) as CJ
		LEFT JOIN #tempPlacements_VW as P
		ON CJ.CalendarQuarter=P.CalendarQuarter
		AND CJ.PlacedFromLocation=P.PlacedFromLocation 
		and CJ.WaitTimeGroup = P.WaitTimeGroup
		WHERE (CJ.PlacedFromLocationCategory ='Acute' AND CJ.PlacedFromLocation='Acute') OR
		(CJ.PlacedFromLocationCategory='Community' AND CJ.PlacedFromLocation not in ('Acute','Outside HSDA')) OR
		(CJ.PlacedFromLocationCategory ='Outside HSDA' AND CJ.PlacedFromLocation='Outside HSDA')
	END

	-----------------------------------------------
	-- ID09 # RC priority Access Placements from acute within target rate
	-----------------------------------------------
		/*
		Purpose: To compute the number of placements from location categories, as well as the percentage of cases placed within target wait times.
		Stores the data into a table for the SSRS report to pull from.
		Needs to be run for each update
		Author: Hans Aisake
		Date Created: November 24, 2017
		Date Modified: Oct 19, 2018
		Inclusions/Exclusions: 
		-Includes palcements from the Richmond Priority Access Wait List only.
		Comments:
		-Removed Community and MH&A grouping on March 8, 2018.
		-wait time is Date on to Date off.
		-These wait times do include suspended time.
		-targets were given by Alfonso and Natalie.
		*/

	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID09') IS NOT NULL DROP TABLE #VW_ID09
	END

	BEGIN
		SELECT 	9 as 'IndicatorID'
		, 'Richmond PA' as 'Waitlist'
		, 'Residential Care' as 'Program'
		, CalendarQuarterEndDate as 'TimeFrame'
		, CalendarQuarter as 'TimeFrameLabel'
		, 'CalendarQuarter' as 'TimeFrameType'
		, '% Placements within Target(30days) from VCH Acute' as 'IndicatorName'
		, SUM(NumPlacedWithinTarget) as 'Numerator'
		, SUM(NumPlacements) as 'Denominator'
		, IIF(SUM(NumPlacements) =0, NULL, 1.0*SUM(NumPlacedWithinTarget)/SUM(NumPlacements)) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'P0' as 'Format'
		, AVG([target]) as 'Target'
		, 'CommunityMart' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID09
		FROM #tempPlacements_VW2
		WHERE PlacedFromLocationCategory='Acute'
		GROUP BY CalendarQuarter, CalendarQuarterEndDate
		HAVING SUM(NumPlacements)>0		--not sure what to do with this
	END

	-----------------------------------------------
	-- ID10 # RC priority Access Placements from Community within target rate
	-----------------------------------------------
	/*
		Purpose: To compute the number of placements from location categories within target timeframes
		Author: Hans Aisake
		Date Created: November 24, 2017
		Date Modified: Oct 19, 2018
		Inclusions/Exclusions: 
		-Includes palcements from the Richmond Priority Access Wait List only.
		Comments:
		-Removed Community and MH&A grouping on March 8, 2018.

		-wait time is Date on to Date off.
		-These wait times do include suspended time.
		-targets were given by Alfonso and Natalie.
	*/
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID10') IS NOT NULL DROP TABLE #VW_ID10
	END

	BEGIN
		SELECT 	10 as 'IndicatorID'
		, 'Richmond PA' as 'Waitlist'
		, 'Residential Care' as 'Program'
		, CalendarQuarterEndDate as 'TimeFrame'
		, CalendarQuarter as 'TimeFrameLabel'
		, 'CalendarQuarter' as 'TimeFrameType'
		, '% Placements within Target (30days) from RH Community' as 'IndicatorName'
		, SUM(NumPlacedWithinTarget) as 'Numerator'
		, SUM(NumPlacements) as 'Denominator'
		, IIF(  SUM(NumPlacements) is NULL, NULL, 1.0*SUM(NumPlacedWithinTarget)/SUM(NumPlacements) ) as 'Value'
		, 'Above' as 'DesiredDirection'
		, 'P0' as 'Format'
		, AVG([target]) as 'Target'
		, 'CommunityMart' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID10
		FROM #tempPlacements_VW2
		WHERE PlacedFromLocationCategory='Community'
		GROUP BY CalendarQuarter, CalendarQuarterEndDate
		HAVING SUM(NumPlacements)>0
	END


-----------------------------------------------
-- ID13 Percent of surgical Patients Treated Within Target Wait Time
-----------------------------------------------
	/*	Purpose: to compute the percentage of surgeries completed within the target wait time. Wailist for surgery to surgery performed.
		Author: Kaloupis Peter
		Co-author: Hans Aisake
		Date Created: 2016
		Date Modified: 
		Inclusions/Exclusions: 
		Comments:
	*/
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_excludeORCodes') IS NOT NULL DROP TABLE #VW_excludeORCodes;
		CREATE TABLE #VW_excludeORCodes	( codes int)
	END

	BEGIN
		INSERT INTO #VW_excludeORCodes VALUES(11048),(12001),(12002),(12003),(12004),(12005),(12006),(12007)
		,(12008),(12009),(20135),(30007),(40018),(40033),(10161),(10163),(20123)
		,(20124),(40029),(20012),(20049),(20138),(40040)
	END

	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID13') IS NOT NULL DROP TABLE #VW_ID13;
	END

	BEGIN
		SELECT 	13 as 'IndicatorID'
		, O.facilityLongName as 'Facility'
		,  REPLACE(REPLACE( O.LoggedMainSurgeonSpecialty, CHAR(13), ''), CHAR(10), '') as 'LoggedMainsurgeonSpecialty'
		, T.thursdaywkend as 'TimeFrame'
		, CONVERT(varchar,CONVERT(date,T.thursdaywkend),120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, '% Surgical Patients Treated Within Target Wait Time' as 'IndicatorName'
		, sum(cast(O.ismeetingtarget as int)) as 'Numerator'
		, count(*) as 'Denominator'
		, 1.0*sum(cast(O.ismeetingtarget as int))/count(*)  as 'Value'
		, 'Above' as 'DesiredDirection'
		, 'P1' as 'Format'
		, 0.85 as 'Target'
		, 'ORMart' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID13
		FROM ORMARt.[dbo].[vwRegionalORCompletedCase] as O
		INNER JOIN #VW_weekReportTF as T
		ON O.SurgeryPerformedDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd	--surgeries performed in thursday week. Personally, I'd be more interested in waits starting than completion perspectives.
		LEFT JOIN #VW_excludeORCodes as C
		ON O.[LoggedSPRPx1Code]=C.codes		--only keep codes 		('11048','12001','12002','12003','12004','12005','12006','12007','12008','12009','20135','30007','40018','40033','10161','10163','20123','20124','40029','20012','20049','20138','40040')
		WHERE O.facilitylongname='richmond hospital'	--richmond only
		and O.IsScheduled = 1		--only include scheduled surgeries 
		and ORRoomCode in ('RH BC','RH PRIVGOV','RHOR1','RHOR2','RHOR3','RHOR4','RHOR5','RHOR6','RHOR7','RHOR8','RHPRIV')	--only include these OR room codes for Richmond
		AND C.Codes is NULL	--exclude these procedures
		group by O.facilityLongName
		, T.thursdayWkEnd
		, REPLACE(REPLACE( LoggedMainSurgeonSpecialty, CHAR(13), ''), CHAR(10), '') 
		--add overall
		UNION
		SELECT 	13 as 'IndicatorID'
		, O.facilityLongName as 'Facility'
		,  'All Services' as 'LoggedMainsurgeonSpecialty'
		, T.thursdaywkend as 'TimeFrame'
		, CONVERT(varchar,CONVERT(date,T.thursdaywkend),120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, '% Surgical Patients Treated Within Target Wait Time' as 'IndicatorName'
		, sum(cast(O.ismeetingtarget as int)) as 'Numerator'
		, count(*) as 'Denominator'
		, 1.0*sum(cast(O.ismeetingtarget as int))/count(*)  as 'Value'
		, 'Above' as 'DesiredDirection'
		, 'P1' as 'Format'
		, 0.85 as 'Target'
		, 'ORMart' as 'DataSource'
		, 1 as 'IsOverall'
		FROM ORMARt.[dbo].[vwRegionalORCompletedCase] as O
		INNER JOIN #VW_weekReportTF as T
		ON O.SurgeryPerformedDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd	--surgeries performed in thursday week. Personally, I'd be more interested in waits starting than completion perspectives.
		INNER JOIN #VW_excludeORCodes as C
		ON O.[LoggedSPRPx1Code]=C.codes		--only keep codes 		('11048','12001','12002','12003','12004','12005','12006','12007','12008','12009','20135','30007','40018','40033','10161','10163','20123','20124','40029','20012','20049','20138','40040')
		WHERE O.facilitylongname='richmond hospital'	--richmond only
		and O.IsScheduled = 1		--only include scheduled surgeries
		and ORRoomCode in ('RH BC','RH PRIVGOV','RHOR1','RHOR2','RHOR3','RHOR4','RHOR5','RHOR6','RHOR7','RHOR8','RHPRIV')	--only include these OR room codes for Richmond
		AND C.Codes is NULL	--exclude these procedures
		group by T.thursdayWkEnd,
		O.FacilityLongName
	END

-----------------------------------------------
-- ID14 Direct Discharges From ED
-----------------------------------------------
	/*	Purpose: to compute how many DDFEs we have every week
		Author: Hans Aisake
		Date Created: 2017
		Date Modified: June 19, 2018
		Inclusions/Exclusions: 
		Comments:
	*/
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID14') IS NOT NULL DROP TABLE #VW_ID14
	END

	BEGIN
		SELECT 	14 as 'IndicatorID'
		, FacilityLongName as 'Facility'
		, CASE WHEN ED.[age]>'69' THEN '70+ years'  
			   ELSE '0-69 years' 
		END as 'Age_Group'
		, T.ThursdayWkEnd as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,T.ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'timeFrameType'
		, '# Direct Discharges from ED' as 'IndicatorName'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, Count(distinct ED.VisitId)  as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		, 0 as 'Target'
		, 'EDMart' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID14
		FROM EDMART.[dbo].[vwEDVisitIdentifiedRegional] as ED
		INNER JOIN #VW_weekReportTF as T
		ON ED.Dispositiondate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd		--link ED visit dispositions to thursday weeks and only keep those of interest in the reporting time frame
		WHERE ED.[InpatientNursingUnitName] ='Invalid'		--this is populated with invalid because there was a bed request but not proper inpatient unit was noted
		AND ED.facilityshortname='RHS'						--only richmond ED visits
		GROUP BY T.ThursdayWkEnd
		, FacilityLongName
		,CASE WHEN ED.[age]>'69' THEN '70+ years' 
			  ELSE '0-69 years' 
		END
		--add overall
		UNION
		SELECT  14 as 'IndicatorID'
		, FacilityLongName as 'Facility'
		, 'Overall' as 'Age_Group'
		, T.ThursdayWkEnd as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,T.ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'timeFrameType'
		, '# Direct Discharges from ED' as 'IndicatorName'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, Count(distinct ED.VisitId)  as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		, 0 as 'Target'
		, 'EDMart' as 'DataSource'
		, 1 as 'IsOverall'
		FROM EDMART.[dbo].[vwEDVisitIdentifiedRegional] as ED
		INNER JOIN #VW_weekReportTF as T
		ON ED.Dispositiondate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd		--link ED visit dispositions to thursday weeks and only keep those of interest in the reporting time frame
		WHERE ED.[InpatientNursingUnitName] ='Invalid'		--this is populated with invalid because there was a bed request but not proper inpatient unit was noted
		AND ED.facilityshortname='RHS'					--only richmond ED visits
		GROUP BY T.ThursdayWkEnd
		, FacilityLongName
	END

	BEGIN
		--append 0s
		INSERT INTO #VW_ID14 (IndicatorID,Facility,IndicatorName,Age_Group,TimeFrame,TimeFrameLabel,TimeFrameType,Numerator,Denominator,[Value],DesiredDirection,[Format],[Target],DataSource,IsOverall)
		SELECT 	14 as 'IndicatorID'
		, p.facility
		, p.IndicatorName
		, P.entity_group as 'Age_Group'
		, P.TimeFrame
		, CONVERT(VARCHAR, CONVERT(date,P.TimeFrame), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'TimeFrameType'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, 0 as 'Value'	--proper 0's
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		, 0  as 'Target'
		, 'ADTCMart' as 'DataSource'
		, CASE WHEN P.entity_group='Overall' THEN 1 ELSE 0 END as 'IsOverall'
		FROM #placeholder as P
		LEFT JOIN #VW_ID14 as I
		ON P.facility=I.Facility
		AND P.IndicatorName=I.IndicatorName
		AND P.entity_group=I.Age_Group
		AND P.TimeFrame=I.TimeFrame
		WHERE P.IndicatorName = (SELECT distinct IndicatorName FROM #VW_ID14)
		AND I.[Value] is NULL
	END

-----------------------------------------------
-- ID15 Average Census
-----------------------------------------------
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_census_15') IS NOT NULL DROP TABLE #VW_census_15
	END

	BEGIN
		SELECT C.accountnum
		, T.ThursdayWkEnd
		--, C.PatientServiceDescription
		--, C.[NursingUnitDesc]
		, C.FacilityLongName
		, ISNULL(MAP.NewProgram,'Unknown') as 'Program'
		INTO #VW_census_15
		FROM ADTCMart.[ADTC].[vwCensusFact] as C					--to pull nursing unit classification to focus on inpatient units
		INNER JOIN #adtcNUClassification_VW as NU
		ON C.NursingUnitCode = NU.NursingUnitCode
		INNER JOIN #VW_weekReportTF as T							--only keep census for the reporting time frame of interest
		ON C.CensusDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd
		LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP
		ON C.NursingUnitCode=MAP.nursingunitcode					--same unit
		AND C.CensusDate BETWEEN MAP.StartDate AND MAP.EndDate		--unit-program active mapping dates
		WHERE C.[site] ='rmd'				--richmond only
		AND NU.NULevel='Acute'				--inpatient units only
		AND C.[PatientServiceCode]<>'NB'	--exclude newbornds
		AND C.[AccountType]='Inpatient'		--inpatients only
		AND C.[AccountSubType]='Acute'		--inpatients only
	END

	--compute and store indicators
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID15') IS NOT NULL DROP TABLE #VW_ID15
	END

	BEGIN
		SELECT 	15 as 'IndicatorID'
		, FacilityLongName as 'Facility'
		, [Program] as 'Program'
		, ThursdayWkEnd as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'timeFrameType'
		, 'Inpatient Census' as 'IndicatorName'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, Count(*)/7  as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		,  NULL as 'Target'
		, 'Census' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID15
		FROM #VW_census_15
		GROUP BY FacilityLongName
		, Program
		, ThursdayWkEnd
		--add overall
		UNION
		SELECT 	15 as 'IndicatorID' 
		, FacilityLongName as 'Facility'
		, 'Overall' as 'Program'
		, ThursdayWkEnd as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'timeFrameType'
		, 'Inpatient Census' as 'IndicatorName'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, Count(*)/7  as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		,  233 as 'Target'
		, 'Census' as 'DataSource'
		, 1 as 'IsOverall'
		FROM #VW_census_15
		GROUP BY FacilityLongName
		, ThursdayWkEnd
	END

	--append 0s
	BEGIN
		INSERT INTO #VW_ID15 (IndicatorID, Facility,IndicatorName,Program,TimeFrame,TimeFrameLabel,TimeFrameType,Numerator,Denominator,[Value],DesiredDirection,[Format],[Target],DataSource, IsOverall)
		SELECT	15 as 'IndicatorID' 
		, p.facility
		, p.IndicatorName
		, P.entity_group as 'Age_Group'
		, P.TimeFrame
		, CONVERT(VARCHAR, CONVERT(date,P.TimeFrame), 120) as 'TimeFrameLabel'
		, 'Weekly'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, 0 as 'Value'	--proper 0's
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		, CASE WHEN Program!='Overall' THEN NULL ELSE 233 END as 'Target'
		, 'ADTCMart' as 'DataSource'
		, CASE WHEN P.entity_group='Overall' THEN 1 ELSE 0 END as 'IsOverall'
		FROM #placeholder as P
		LEFT JOIN #VW_ID15 as I
		ON P.facility=I.Facility
		AND P.IndicatorName=I.IndicatorName
		AND P.entity_group=I.Program
		AND P.TimeFrame=I.TimeFrame
		WHERE P.IndicatorName = (SELECT distinct IndicatorName FROM #VW_ID15)
		AND I.[Value] is NULL
	END

-----------------------------------------------
-- ID16 Total ALC spent waiting for RC
-----------------------------------------------
/*
Purpose: Pull the number of ALC days spent waiting for RC; both directly and for assessment
Author: Hans aisake
Date Created: Oct 19, 2018
Date Modified: 
Comments: 
*/
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_alcRC_census_16') IS NOT NULL DROP TABLE #VW_alcRC_census_16
	END

	BEGIN
		SELECT C.accountnum
		, T.ThursdayWkEnd
		, C.PatientServiceDescription
		, C.[NursingUnitDesc]
		, C.FacilityLongName
		, ISNULL(MAP.NewProgram,'Unknown') as 'Program'
		INTO #VW_alcRC_census_16
		FROM ADTCMart.[ADTC].[vwCensusFact] as C					--to pull nursing unit classification to focus on inpatient units
		INNER JOIN #adtcNUClassification_VW as NU
		ON C.NursingUnitCode = NU.NursingUnitCode
		INNER JOIN #VW_weekReportTF as T							--only keep census for the reporting time frame of interest
		ON C.CensusDate BETWEEN T.ThursdayWkStart AND T.ThursdayWkEnd
		LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as MAP
		ON	C.NursingUnitCode =MAP.nursingunitcode				--same nursing unit code
		AND C.CensusDate BETWEEN MAP.StartDate AND MAP.EndDate	--acitve unit-program mapping dates
		WHERE C.[Site]='rmd'		--richmond only
		AND NU.NULevel='Acute'		--inpatient units only
		AND C.[PatientServiceCode]<>'NB'	--exclude newborns
		AND C.[AccountType]='Inpatient'		--inpatients only
		AND C.PatientServiceCode in ('AL0','AL10')	--census date flagged as alc
	END

	--compute and store indicators
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID16') IS NOT NULL DROP TABLE #VW_ID16
	END

	BEGIN
		SELECT	16 as 'IndicatorID' 
		, FacilityLongName as 'Facility'
		, [Program] as 'Program'
		, ThursdayWkEnd as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'timeFrameType'
		, 'Total ALC days for RC (AL0,AL10)' as 'IndicatorName'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, Count(distinct AccountNum)  as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		,  NULL as 'Target'
		, 'Census' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID16
		FROM #VW_alcRC_census_16
		GROUP BY FacilityLongName
		, Program
		, ThursdayWkEnd
		--add overall
		UNION
		SELECT 	16 as 'IndicatorID' 
		, FacilityLongName as 'Facility'
		, 'Overall' as 'Program'
		, ThursdayWkEnd as 'TimeFrame'
		, CONVERT(VARCHAR, CONVERT(date,ThursdayWkEnd), 120) as 'TimeFrameLabel'
		, 'Weekly' as 'timeFrameType'
		, 'Total ALC days for RC (AL0,AL10)' as 'IndicatorName'
		, NULL as 'Numerator'
		, NULL as 'Denominator'
		, Count(distinct AccountNum)  as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D0' as 'Format'
		,  NULL as 'Target'
		, 'Census' as 'DataSource'
		, 1 as 'IsOverall'
		FROM #VW_alcRC_census_16
		GROUP BY FacilityLongName
		, ThursdayWkEnd
	END

---------------------------------------
-- ID17 Turn Over
--------------------------------------
	/*
	Purpose: To pull the turn over volumes and rates from the EE extract table in DSSI.
	Author: Hans Aisake
	Date Created: October 3, 2018
	Date Modified: 
	Inclusions/Exclusions: The turn over categories are repackaged using a custom logic here.
	Retirement, conversion to casual positions, went on leave, and suspensions are excluded. These events are rarer.
	Comments:
	*/

	--clear out data in a reformated and repacked format of the turn over data; this table is used for a custom chart as well as further computations
	BEGIN
		TRUNCATE TABLE  DSSI.dbo.RH_VisibilityWall_EE_Turnover
	END
	
	BEGIN
		--reformat and repacked the data
		INSERT INTO DSSI.dbo.RH_VisibilityWall_EE_Turnover (FiscalPeriodLong, FiscalPeriodEndDate, ProgramDesc, TurnoverType, TurnoverVolume,  ProgramHeadcount)
		SELECT FiscalPeriodLong
		, FiscalPeriodEndDate
		, ProgramDesc
		,'LeavingVCH' as 'TurnoverType'
		,SUM([Leaving VCH Involuntarily]+[Leaving VCH Voluntarily]) as 'TurnoverVolume'
		,SUM(Headcount) as 'ProgramHeadcount'
		FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
		,ProgramDesc
		UNION
		--leaving department
		SELECT FiscalPeriodLong
		, FiscalPeriodEndDate
		, ProgramDesc
		,'LeavingDepartment' as 'TurnoverType'
		,SUM([Transfer Out of Department (CC)]) as 'TurnoverVolume'
		,SUM(Headcount) as 'ProgramHeadcount'
		FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
		, ProgramDesc
		--Leaving COC
		UNION
		SELECT FiscalPeriodLong
		, FiscalPeriodEndDate
		,ProgramDesc
		,'LeavingCoC' as 'TurnoverType'
		,SUM([Transfer out of Richmond (CoC)]) as 'TurnoverVolume'
		,SUM(Headcount) as 'ProgramHeadcount'
		FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
		,ProgramDesc
	END

	--clear out the results table for recreation
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID17') IS NOT NULL DROP TABLE #VW_ID17;
	END

	BEGIN
		SELECT 	17 as 'IndicatorID'
		, 'Richmond' as 'Facility'
		, ProgramDesc as 'Program'
		, FiscalPeriodEndDate as 'TimeFrame'
		, FiscalPeriodLong as 'TimeFrameLabel'
		, 'Fiscal Period' as 'TimeFrameType'
		, 'Turnover Rate' as 'IndicatorName'
		, SUM(TurnoverVolume) as 'Numerator'
		, AVG(ProgramHeadCount) as 'Denominator'
		, 1.0*SUM(TurnoverVolume)/AVG(ProgramHeadCount) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D1' as 'Format'
		, NULL as 'Target'
		, 'EE Extract' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID17
		FROM  DSSI.dbo.RH_VisibilityWall_EE_Turnover
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY ProgramDesc
		, FiscalPeriodEndDate
		, FiscalPeriodLong
		--add overall
		UNION
		SELECT 	17 as 'IndicatorID'
		, 'Richmond' as 'Facility'
		, 'Overall' as 'Program'
		, X.FiscalPeriodEndDate as 'TimeFrame'
		, X.FiscalPeriodLong as 'TimeFrameLabel'
		, 'Fiscal Period' as 'TimeFrameType'
		, 'Turnover Rate' as 'IndicatorName'
		, SUM(X.TurnoverVolume) as 'Numerator'
		, AVG(Y.TotalHeadCount) as 'Denominator'
		, 1.0*SUM(X.TurnoverVolume)/AVG(Y.TotalHeadCount) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D1' as 'Format'
		, NULL as 'Target'
		, 'EE Extract' as 'DataSource'
		, 1 as 'IsOverall'
		FROM  DSSI.dbo.RH_VisibilityWall_EE_Turnover as X
		LEFT JOIN
		--need a seperate table with overall headcount as it can't be computed from DSSI.[dbo].[RH_VisibilityWall_EE_Turnover]
		(
			SELECT FiscalPeriodLong
			,SUM(Headcount) as 'TotalHeadCount'
			FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
			GROUP BY FiscalPeriodLong
		) as Y
		ON X.FiscalPeriodLong=Y.FiscalPeriodLong
		WHERE X.fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY X.FiscalPeriodEndDate
		, X.FiscalPeriodLong
	END

---------------------------------------
-- ID18 Sick Rate
---------------------------------------
	/*
	Purpose: To pull the sick hours and productive horus from the EE extrat data to compute a sick ratio
	Author: Hans Aisake
	Date Created: October 3, 2018
	Date Modified:
	Inclusions/Exclusions:
	Comments:
		EE has mentioned that the productive hours has errors in computation because their data can't keep up with the shift adjustments done by opperations.
	*/
	--clear out the results table for recreation
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_ID18') IS NOT NULL DROP TABLE #VW_ID18;
	END

	BEGIN
		SELECT 18 as 'IndicatorID'
		, 'Richmond' as 'Facility'
		, ProgramDesc as 'Program'
		, FiscalPeriodEndDate as 'TimeFrame'
		, FiscalPeriodLong as 'TimeFrameLabel'
		, 'Fiscal Period' as 'TimeFrameType'
		, 'Sick Ratio' as 'IndicatorName'
		, SUM([Sick Time]) as 'Numerator'
		, CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END as 'Denominator'
		, 1.0*SUM([Sick Time])/(CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D1' as 'Format'
		, 0.048 as 'Target'
		, 'EE Extract' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID18
		FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
		,ProgramDesc
		--add overall
		UNION
		SELECT 18 as 'IndicatorID'
		, 'Richmond' as 'Facility'
		, 'Overall' as 'Program'
		, FiscalPeriodEndDate as 'TimeFrame'
		, FiscalPeriodLong as 'TimeFrameLabel'
		, 'Fiscal Period' as 'TimeFrameType'
		, 'Sick Ratio' as 'IndicatorName'
		, SUM([Sick Time]) as 'Numerator'
		, CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END as 'Denominator'
		, 1.0*SUM([Sick Time])/(CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D1' as 'Format'
		, 0.048 as 'Target'
		, 'EE Extract' as 'DataSource'
		, 1 as 'IsOverall'
		FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
	END

--------------------------------------
-- ID19 OT rate
---------------------------------------
	/*
	Purpose: To pull the overtime hours and productive horus from the EE extrat data to compute a overtime ratio
	Author: Hans Aisake
	Date Created: October 3, 2018
	Date Modified:
	Inclusions/Exclusions:
	Comments:
		EE has mentioned that the productive hours has errors in computation because their data can't keep up with the shift adjustments done by opperations.
	*/
	
	BEGIN
	--clear out the results table for recreation
		IF OBJECT_ID('tempdb.dbo.#VW_ID19') IS NOT NULL DROP TABLE #VW_ID19;
	END

	BEGIN
		SELECT  19 as 'IndicatorID'
		, 'Richmond' as 'Facility'
		, ProgramDesc as 'Program'
		, FiscalPeriodEndDate as 'TimeFrame'
		, FiscalPeriodLong as 'TimeFrameLabel'
		, 'Fiscal Period' as 'TimeFrameType'
		, 'Overtime Ratio' as 'IndicatorName'
		, SUM([Overtime])  as 'Numerator'
		, (CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END) as 'Denominator'
		, 1.0*SUM([Overtime]) /(CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D1' as 'Format'
		, 0.022 as 'Target'
		, 'EE Extract' as 'DataSource'
		, 0 as 'IsOverall'
		INTO #VW_ID19
		FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
		,ProgramDesc
		--add overall
		UNION
		SELECT  19 as 'IndicatorID'
		, 'Richmond' as 'Facility'
		, 'Overall' as 'Program'
		, FiscalPeriodEndDate as 'TimeFrame'
		, FiscalPeriodLong as 'TimeFrameLabel'
		, 'Fiscal Period' as 'TimeFrameType'
		, 'Overtime Ratio' as 'IndicatorName'
		, SUM([Overtime])  as 'Numerator'
		, (CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END) as 'Denominator'
		, 1.0*SUM([Overtime]) /(CASE WHEN SUM([Productive Hours])=0 THEN 1 ELSE SUM([Productive Hours]) END) as 'Value'
		, 'Below' as 'DesiredDirection'
		, 'D1' as 'Format'
		, 0.022 as 'Target'
		, 'EE Extract' as 'DataSource'
		, 1 as 'IsOverall'
		FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
		WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
	END
 
	-----------------------------------------
	-- ID20 Percentage of Open Postings
	-----------------------------------------
	/*
	Purpose: Link the Open Posting data from EE to finance program description and calculates open postings/headcount.
	Author: Hans Aisake
	Date Created: Oct 3, 2018
	Date Modified: 
	Inclusions/Exclusions: None
	Comments:
	*/
		BEGIN
			--clear out the results table for recreation
			IF OBJECT_ID('tempdb.dbo.#VW_ID20') IS NOT NULL DROP TABLE #VW_ID20;
		END

		BEGIN
			SELECT 20 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, ProgramDesc as 'Program'
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'Percentage of Open Postings ' as 'IndicatorName'
			, SUM([# of postings]) as 'Numerator'
			, SUM(headcount) as 'Denominator'
			, 1.0*SUM([# of postings])/SUM(headcount) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D0' as 'Format'
			, 0.05 as 'Target'
			, 'EE Extract' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID20
			FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
			WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
			GROUP BY FiscalPeriodLong
			, FiscalPeriodEndDate
			,ProgramDesc
			--add overall
			UNION
			SELECT 20 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, 'Overall' as 'Program'
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'Percentage of Open Postings ' as 'IndicatorName'
			, SUM([# of postings]) as 'Numerator'
			, SUM(headcount) as 'Denominator'
			, 1.0*SUM([# of postings])/SUM(headcount) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D0' as 'Format'
			, 0.05 as 'Target'
			, 'EE Extract' as 'DataSource'
			, 1 as 'IsOverall'
			FROM DSSI.dbo.RH_VisibilityWall_EE_Extract
			WHERE fiscalperiodenddate < GETDATE()	--only compelted fiscal periods
			GROUP BY FiscalPeriodLong
			, FiscalPeriodEndDate
		END
	
	-----------------------------------------------
	-- ID21 CSAEs Overall, ID22 UTI, ID23 Pneumonia
	-----------------------------------------------
	/*
	Purpose: pull UTI and Pneumonia data for the visibility wall reports.
	Author: Peter Kaloupis
	Co-author: Hans Aisake
	Date Created: 2015
	Date Modified: July 19, 2017
	Inclusions/exclusions: built into the source table.
	Only 55+ medical/surgical combined patients.
	Comments: Program names changed around 2017/2018-P3. It is currently manualy overwritten back to the old names.

	*/

		--identify the last 26 FPs with CSAE data
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_fp_21') IS NOT NULL DROP TABLE #VW_fp_21;
		END

		BEGIN
			SELECT DISTINCT TOP 26 fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate 
			INTO #VW_fp_21
			FROM adrmart.dim.[date] 
			WHERE fiscalperiodenddate <= (SELECT MAX(dischargedate) from adrmart.dbo.UTIPneumoniaSummaryData)
			ORDER BY fiscalperiodenddate DESC
		END

		--create placeholder data set with all timeframes and programs for 0 inclusion
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_CASE_results') IS NOT NULL DROP TABLE #VW_CASE_results;
		END

		BEGIN
			SELECT D.*, P.*
			INTO #VW_CASE_results
			FROM #VW_fp_21 as D
			CROSS JOIN (SELECT DISTINCT ProgramDesc FROM adrmart.dbo.UTIPneumoniaSummaryData where DADInstitutionCode='rmd' ) as P
		END

		--process the CSAE data to compute metrics
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_data_21') IS NOT NULL DROP TABLE #VW_data_21;
		END

		--compute the number of cases
		BEGIN
			SELECT R.[FiscalPeriodLong]
			, R.FiscalPeriodEndDate
			, ISNULL(R.[ProgramDesc],'Unknown') as 'Program'
			, DATEDIFF(day, R.[FiscalPeriodStartDate],R.[FiscalPeriodENDDate])+1 as 'daysInFP'
			,SUM(CASE WHEN X.[UTIFlag]='Y' then 1 ELSE 0 END) as 'UTICASES'
			,SUM(CASE WHEN X.[PneumoniaFlag]='Y' then 1 ELSE 0 END) as 'PneumoniaCASES'
			,SUM(CASE WHEN X.[UTIFlag]='Y' or X.[PneumoniaFlag]='Y' then 1 ELSE 0 END) as 'UTI+PneumoniaCASES'
			,COUNT(1) as 'NumDischarges'
			,CASE WHEN R.fiscalperiodlong between '2014-01' and '2014-09' then 5.0 
				  WHEN R.fiscalperiodlong between '2014-10' and '2015-10' then 13.6
				  WHEN R.fiscalperiodlong between '2015-11' and '2016-10' then 13.1 
				  WHEN R.fiscalperiodlong >= '2016-11' then 9.0
				  ELSE null 
			END as 'UTITargetRate'
			,CASE WHEN R.fiscalperiodlong between '2014-01' and '2014-09' then 3.2 
				  WHEN R.fiscalperiodlong between '2014-10' and '2015-10' then 11.2
				  WHEN R.fiscalperiodlong between '2015-11' and '2016-10' then 10.3
				  WHEN R.fiscalperiodlong >= '2016-11' then 9.1
				  ELSE null 
			END as 'UTIandPneumoniaTargetRate'
			,CASE  WHEN R.fiscalperiodlong >= '2016-11' then 9.1
				  ELSE null 
			END as 'PneumoniaTargetRate'
			,CASE  WHEN  R.fiscalperiodlong >='2017-11' then (28.0/(DATEDIFF(day, R.[FiscalPeriodStartDate], R.[FiscalPeriodENDDate])+1)*12.0)
				   ELSE null 
			END as 'UTIandPneumoniaCountTarget'
			INTO #VW_data_21
			FROM #VW_CASE_results as R
			LEFT JOIN adrmart.dbo.UTIPneumoniaSummaryData as X		--Left join is very important to get 0's
			ON X.dischargedate BETWEEN R.FiscalPeriodStartDate AND R.FiscalPeriodENDDate
			AND X.ProgramDesc=R.ProgramDesc AND X.DADInstitutionCode='rmd'
			GROUP BY R.FiscalPeriodLong, R.FiscalPeriodStartDate, R.FiscalPeriodEndDate, R.ProgramDesc
		END
	
		--UTI + Pneumonia Rate indicators
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_ID21') IS NOT NULL DROP TABLE #VW_ID21;
		END

		BEGIN
			SELECT 21 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, Program
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'UTI + Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)' as 'IndicatorName'
			, [UTI+PneumoniaCASEs] as 'Numerator'
			, NumDischarges as 'Denominator'
			, 1000.0*[UTI+PneumoniaCASEs]/NumDischarges*daysInFP/28 as 'Value'	--weighted by days in the FP
			, 'Below' as 'DesiredDirection'
			, 'P1' as 'Format'
			,  Null as 'Target'
			, 'ADRMart' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID21
			FROM #VW_Data_21
			--add overall
			UNION
			SELECT 21 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, 'Overall' as 'Program'
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'UTI + Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)' as 'IndicatorName'
			, SUM([UTI+PneumoniaCASEs]) as 'Numerator'
			, SUM(NumDischarges) as 'Denominator'
			, 1000.0*SUM([UTI+PneumoniaCASEs])/SUM(NumDischarges)*AVG(daysInFP)/28 as 'Value'	--weighted by days in the FP
			, 'Below' as 'DesiredDirection'
			, 'P1' as 'Format'
			,  AVG(UTIandPneumoniaTargetRate) as 'Target'
			, 'ADRMart' as 'DataSource'
			, 1 as 'IsOverall'
			FROM #VW_Data_21
			GROUP BY FiscalPeriodLong, FiscalPeriodEndDate
		END

		--UTI 
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_ID22') IS NOT NULL DROP TABLE #VW_ID22;
		END

		BEGIN
			SELECT 22 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, Program
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'UTI Rate per 1,000 Discharges (55+ medical/surgical)' as 'IndicatorName'
			, [UTICASES] as 'Numerator'
			, NumDischarges as 'Denominator'
			, 1000.0*[UTICASES]/NumDischarges*daysInFP/28  as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'P1' as 'Format'
			,  Null as 'Target'
			, 'ADRMart' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID22
			FROM #VW_Data_21
			--add overall
			UNION
			SELECT 22 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, 'Overall' as 'Program'
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'UTI Rate per 1,000 Discharges (55+ medical/surgical)' as 'IndicatorName'
			, SUM([UTICASES]) as 'Numerator'
			, SUM(NumDischarges) as 'Denominator'
			, 1000.0*SUM([UTICASES])/SUM(NumDischarges)*AVG(daysInFP)/28  as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'P1' as 'Format'
			,  AVG(UTITargetRate) as 'Target'
			, 'ADRMart' as 'DataSource'
			, 1 as 'IsOverall'
			FROM #VW_Data_21
			GROUP BY FiscalPeriodLong, FiscalPeriodEndDate
			;
		END

		--Pneumonia
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_ID23') IS NOT NULL DROP TABLE #VW_ID23;
		END

		BEGIN
			SELECT 23 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, Program
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)' as 'IndicatorName'
			, [PneumoniaCASES] as 'Numerator'
			, NumDischarges as 'Denominator'
			, 1000.0*[PneumoniaCASES]/NumDischarges*daysInFP/28 as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'P1' as 'Format'
			,  Null as 'Target'
			, 'ADRMart' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID23
			FROM #VW_Data_21
			--add overall
			UNION
			SELECT 3 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, 'Overall' as 'Program'
			, FiscalPeriodEndDate as 'TimeFrame'
			, FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)' as 'IndicatorName'
			, SUM([PneumoniaCASES]) as 'Numerator'
			, SUM(NumDischarges) as 'Denominator'
			, 1000.0*SUM([PneumoniaCASES])/SUM(NumDischarges)*AVG(daysInFP)/28 as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'P1' as 'Format'
			,  AVG(PneumoniaTargetRate) as 'Target'
			, 'ADRMart' as 'DataSource'
			, 1 as 'IsOverall'
			FROM #VW_Data_21
			GROUP BY FiscalPeriodLong, FiscalPeriodEndDate
		END
	
	-----------------------------------------------
	-- ID24 Number of Falls Total, ID25 Number of Falls with Harm, ID26 Number of Falls without Harm
	-----------------------------------------------
	/*
	Purpose: Pull Number of Falls for the visibility wall reports.
	Author: Peter Kaloupis
	Co-author: Hans Aisake
	Date Created: 2015 
	Date Modified: 
	Comments: Updated programs and put them in a mapping table in DSSI.
	*/

		--identify the time frames for the falls. Data comes by month and year, so I'm going to report it by month and year. Mapping it to FP is disingenuous
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_falls_tf') IS NOT NULL DROP TABLE #VW_falls_tf;	
		END

		BEGIN
			SELECT distinct TOP 48 [Year], [Month]
			INTO #VW_falls_tf
			FROM [DSSI].[dbo].[RHFallsVisibilityWall]
			ORDER BY [Year] DESC, [Month] DESC
		END

		--identify types of harm
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_falls_harmtypes') IS NOT NULL DROP TABLE #VW_falls_harmtypes; 
		END

		BEGIN
			CREATE TABLE #VW_falls_harmtypes ( HarmFlag varchar(25));
			INSERT INTO #VW_falls_harmtypes 	VALUES ('Harm'),('NoHarm'); 
		END

		--create place holder table to house falls
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_falls_groupings') IS NOT NULL DROP TABLE #VW_falls_groupings; 
		END

		BEGIN
			SELECT T.*,P.*,H.* 	
			INTO #VW_falls_groupings
			FROM #VW_falls_tf as T
			CROSS JOIN  	(SELECT distinct [Director Programs] as 'Program' FROM DSSI.[dbo].[RH_VisibilityWall_QPS_FallsProgramMap]) as P
			CROSS JOIN  	(SELECT * FROM #VW_falls_harmtypes ) as H
		END
	
		--count number of falls resulting in harm or not harm by program
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_falls') IS NOT NULL DROP TABLE #VW_falls;
		END

		BEGIN
			SELECT M.[Director Programs] as 'Program'
			,CASE WHEN [Degree of Harm] in ('1 - No harm','Not Applicable') then 'NoHarm' 
				  ELSE 'Harm' 
			END as 'HarmFlag'
			, [Year]
			, [Month]
			, COUNT(*) as 'NumFallsCases'
			INTO #VW_falls
			FROM [DSSI].[dbo].[RHFallsVisibilityWall] as F
			LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_QPS_FallsProgramMap] as M
			ON F.[Responsible Program]=M.[Falls Responsible Programs]
			WHERE F.[Originated HSDA]='richmond'
			GROUP BY M.[Director Programs]
			,CASE WHEN F.[Degree of Harm] in ('1 - No harm','Not Applicable') then 'NoHarm' 
				  ELSE 'Harm' 
			END
			, [Year]
			, [Month]
		END

		---------------------------------
		--Compile total falls with harm and not harm ID24
		---------------------------------
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_ID24') IS NOT NULL DROP TABLE #VW_ID24;
		END

		BEGIN
			SELECT 24 as 'IndicatorID'
			, 'Richmond' as 'Facility' 
			, R.Program
			, DATEFROMPARTS(R.[Year], R.[Month],1) as 'TimeFrame'	--set to the first of the month but not quite correct. It's easier to specify the start of the month rather than the end.
			, CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END as 'TimeFrameLabel'
			, 'Monthly' as 'TimeFrameType'
			, 'Falls Cases (Total)' as 'IndicatorName'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Numerator'
			, NULL as 'Denominator'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  Null as 'Target'
			, 'BCPSLS' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID24
			FROM #VW_falls_groupings as R
			LEFT JOIN #VW_falls as F
			ON R.[program]=F.[Program]
			AND R.[HarmFlag]=F.[HarmFlag]
			AND R.[Year] = F.[Year] 
			AND R.[Month] = F.[Month]
			GROUP BY R.Program
			, CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END
			, DATEFROMPARTS(R.[Year], R.[Month],1)
			-- add overall
			UNION
			SELECT 24 as 'IndicatorID'
			, 'Richmond' as 'Facility' 
			, 'Overall' as 'Program'
			, DATEFROMPARTS(R.[Year], R.[Month],1) as 'TimeFrame'	--set to the first of the month but not quite correct. It's easier to specify the start of the month rather than the end.
			, CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END as 'TimeFrameLabel'
			, 'Monthly' as 'TimeFrameType'
			, 'Falls Cases (Total)' as 'IndicatorName'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Numerator'
			, NULL as 'Denominator'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  Null as 'Target'
			, 'BCPSLS' as 'DataSource'
			, 1 as 'IsOverall'
			FROM (SELECT distinct [Year], [Month], [HarmFlag] FROM #VW_falls_groupings) as R	--just distinct years, months, and harm
			LEFT JOIN #VW_falls as F
			ON R.[HarmFlag]=F.[HarmFlag]
			AND R.[Year] = F.[Year] 
			AND R.[Month] = F.[Month]
			GROUP BY  CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END
 			, DATEFROMPARTS(R.[Year], R.[Month],1)
		END

		---------------------------------
		--Compile total falls cases with harm ID25
		---------------------------------
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_ID25') IS NOT NULL DROP TABLE #VW_ID25;
		END

		BEGIN
			SELECT 25 as 'IndicatorID'
			, 'Richmond' as 'Facility' 
			, R.Program
			, DATEFROMPARTS(R.[Year], R.[Month],1) as 'TimeFrame'	--set to the first of the month but not quite correct. It's easier to specify the start of the month rather than the end.
			, CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END as 'TimeFrameLabel'
			, 'Monthly' as 'TimeFrameType'
			, 'Falls Cases (Harm)' as 'IndicatorName'
			, ISNULL(F.NumFallsCases,0) as 'Numerator'
			, NULL as 'Denominator'
			, ISNULL(F.NumFallsCases,0) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  Null as 'Target'
			, 'BCPSLS' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID25
			FROM #VW_falls_groupings as R
			LEFT JOIN #VW_falls as F
			ON R.[program]=F.[Program]
			AND R.[HarmFlag]=F.[HarmFlag]
			AND R.[Year] = F.[Year] 
			AND R.[Month] = F.[Month]
			WHERE R.HarmFlag='Harm'
			-- add overall
			UNION
			SELECT 25 as 'IndicatorID'
			, 'Richmond' as 'Facility' 
			, 'Overall' as 'Program'
			, DATEFROMPARTS(R.[Year], R.[Month],1) as 'TimeFrame'	--set to the first of the month but not quite correct. It's easier to specify the start of the month rather than the end.
			, CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END as 'TimeFrameLabel'
			, 'Monthly' as 'TimeFrameType'
			, 'Falls Cases (Harm)' as 'IndicatorName'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Numerator'
			, NULL as 'Denominator'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  Null as 'Target'
			, 'BCPSLS' as 'DataSource'
			, 1 as 'IsOverall'
			FROM (SELECT distinct [Year], [Month], [HarmFlag] FROM #VW_falls_groupings WHERE [HarmFlag]='Harm') as R	--just distinct years, months, and harm
			LEFT JOIN #VW_falls as F
			ON R.[HarmFlag]=F.[HarmFlag]
			AND R.[Year] = F.[Year] 
			AND R.[Month] = F.[Month]
			GROUP BY  CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END
			, DATEFROMPARTS(R.[Year], R.[Month],1)
		END

		---------------------------------
		--Compile total falls cases without harm ID26
		---------------------------------
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_ID26') IS NOT NULL DROP TABLE #VW_ID26;
		END

		BEGIN
			SELECT 26 as 'IndicatorID'
			, 'Richmond' as 'Facility' 
			, R.Program
			, DATEFROMPARTS(R.[Year], R.[Month],1) as 'TimeFrame'	--set to the first of the month but not quite correct. It's easier to specify the start of the month rather than the end.
			, CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END as 'TimeFrameLabel'
			, 'Monthly' as 'TimeFrameType'
			, 'Falls Cases (No Harm)' as 'IndicatorName'
			, ISNULL(F.NumFallsCases,0) as 'Numerator'
			, NULL as 'Denominator'
			, ISNULL(F.NumFallsCases,0) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  Null as 'Target'
			, 'BCPSLS' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID26
			FROM #VW_falls_groupings as R
			LEFT JOIN #VW_falls as F
			ON R.[program]=F.[Program]
			AND R.[HarmFlag]=F.[HarmFlag]
			AND R.[Year] = F.[Year] 
			AND R.[Month] = F.[Month]
			WHERE R.HarmFlag='NoHarm'
			-- add overall
			UNION
			SELECT 26 as 'IndicatorID'
			, 'Richmond' as 'Facility' 
			, 'Overall' as 'Program'
			, DATEFROMPARTS(R.[Year], R.[Month],1) as 'TimeFrame'	--set to the first of the month but not quite correct. It's easier to specify the start of the month rather than the end.
			, CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END as 'TimeFrameLabel'
			, 'Monthly' as 'TimeFrameType'
			, 'Falls Cases (No Harm)' as 'IndicatorName'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Numerator'
			, NULL as 'Denominator'
			, ISNULL(SUM(F.NumFallsCases),0) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  Null as 'Target'
			, 'BCPSLS' as 'DataSource'
			, 1 as 'IsOverall'
			FROM (SELECT distinct [Year], [Month], [HarmFlag] FROM #VW_falls_groupings WHERE [HarmFlag]='NoHarm') as R	--just distinct years, months, and harm
			LEFT JOIN #VW_falls as F
			ON R.[HarmFlag]=F.[HarmFlag]
			AND R.[Year] = F.[Year] 
			AND R.[Month] = F.[Month]
			GROUP BY  CAST(R.[Year] as varchar(4)) + '-' + CASE WHEN LEN(R.[Month])=1 THEN '0' + CAST(R.[Month] as varchar(1)) ELSE CAST(R.[Month] as varchar(2)) END
			, DATEFROMPARTS(R.[Year], R.[Month],1)
		END

	-----------------------------------------------
	-- MRSA/CDI/CPO Rate/Rate/Case per 10,000/10,000/Raw Patient Days ID28 , ID29 , ID30 respectively
	-----------------------------------------------
	/*
	Purpose: Takes the existing weekly MRSA/CDI data query and aggregates the data to fiscal periods instead of by week.
	Then over writes old periods with existing official data.
	Official and unofficial data are flagged via Data Type.
	The table can then be used to generate an SSRS chart in the visibility wall report.

	Author: Hans Aisake
	Co-author: Peter Kaloupis
	Date Create: January 25, 2017
	Date Modified: Sept 24, 2018
	Inclusions/exclusions: excludes TCU census days. only includes inpatient census days
	Comments: This metric needs to be recast in a longer time frame like Fiscal Quarter, but better yet revampped to be time between cases with T or G chart based definitions due to these events being so rare.
    T or G charts would require different data feeds.
	*/
		
		--------------------------------------------------
		--	Reporting Periods
		--------------------------------------------------
		--find reporting periods current fiscal period and the last two fiscal years for the weekly date
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_HAI_tf') IS NOT NULL DROP TABLE #VW_HAI_tf;
		END

		BEGIN
			SELECT distinct TOP 36 fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
			INTO #VW_HAI_tf
			FROM ADRMart.dim.[Date]
			where fiscalperiodenddate <= (select max(WkEnd) from DSSI.dbo.RHMRSACDIVisibilityWall where site='RH')
			ORDER BY fiscalperiodlong DESC
		END

		--------------------------------------------------
		--	results data structure to ensure 0 counts; might not have appropriate histoy cutoffs
		--------------------------------------------------
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_HAI') IS NOT NULL DROP TABLE #VW_HAI;
		END

		BEGIN
			SELECT DISTINCT D.FiscalPeriodLong
			, D.FiscalPeriodStartDate
			, D.FiscalPeriodEndDate
			, P.Program 
			, I.infection
			INTO #VW_HAI
			FROM #VW_HAI_tf as D
			CROSS JOIN (SELECT DISTINCT [NewProgram] as 'Program' FROM DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] WHERE HAI_unitName is not NULL) as P		--only units that showed up in the MRSA/CDI weekly data or the official data are included in the mapping. new units must be added
			CROSS JOIN (SELECT 'CDI' as 'Infection' UNION SELECT 'MRSA' as 'Infection' UNION SELECT 'CPO' as 'Infection') as I
		END
	
		--------------------------------------------------
		--pull the weekly unofficial data. change units to programs. weeks to periods.
		--------------------------------------------------
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_unofficialcases') IS NOT NULL DROP TABLE #VW_unofficialcases;
		END

		BEGIN
			SELECT D.FiscalPeriodLong
			,  CASE WHEN M.NewProgram is NULL THEN 'UNKNOWN' ELSE m.NewProgram END as 'Program'
			, I.[type] as 'Infection'
			, SUM(I.[# of cases]) as 'NumCases'
			INTO #VW_unofficialcases
			FROM DSSI.[dbo].[RHMRSACDIVisibilityWall] as I
			LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC] as M
			ON I.unit=M.HAI_unitname
			left join #VW_HAI_tf as D
			ON I.WkEnd BETWEEN D.fiscalperiodstartdate AND D.FiscalPeriodEndDate
			where I.[site]='rh'
			and I.[type] in ('MRSA', 'CDI','CPO')
			GROUP BY D.FiscalPeriodLong
			, CASE WHEN M.NewProgram is NULL THEN 'UNKNOWN' ELSE m.NewProgram END
			, I.[type]
		END

		--------------------------------------------------
		--Compute IP census
		--------------------------------------------------
			--TCU Census: date and account number for exclusion
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_TCU_census') IS NOT NULL DROP TABLE #VW_TCU_census;
		END

		BEGIN
			SELECT censusDate, AccountNum, [site]
			INTO #VW_TCU_census
			FROM adtcmart.[ADTC].[vwCensusFact]
			WHERE [site]='rmd' AND [AccountType]='Inpatient' AND
			(  ([AccountSubType]='Extended' and [NursingUnitCode]='R3N' and censusdate <='2018-08-24')
			OR ([AccountSubType]='Extended' and [NursingUnitCode]='R4N' and censusdate BETWEEN '2018-08-25' AND '2018-10-17')
			OR ([AccountSubType]='Extended' and [NursingUnitCode]='R3S')
			OR [PatientServiceCode]='TC'
			)
		END

			--pull census days
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_HAI_census') IS NOT NULL DROP TABLE #VW_HAI_census;
		END
	
		BEGIN
			SELECT D.FiscalPeriodLong
			, CASE WHEN M.NewProgram is NULL THEN 'UNKNOWN' ELSE m.NewProgram END as 'Program'
			, COUNT(1) as 'NumIPDays'
			INTO #VW_HAI_census
			FROM adtcmart.[ADTC].[vwCensusFact] as C
			INNER JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC]  as M
			ON C.[NursingUnitCode]=M.[NursingUnitCode]	--only pull census for mapped units
			LEFT JOIN #VW_HAI_tf as D
			ON C.CensusDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
			WHERE C.[FacilityLongName]='richmond hospital'	--Richmond only
			AND C.nursingunitdesc not in ('Minoru Main Floor East','Minoru Main Floor West','Minoru Second Floor East','Minoru Second Floor West','Minoru Third Floor')	--exclude Minoru 
			AND C.[AccountType]='Inpatient'	--inpatients only
			AND not exists (SELECT 1 FROM #VW_TCU_census as TCU WHERE TCU.AccountNum=C.AccountNum AND TCU.CensusDate=C.CensusDate AND TCU.[site]=C.[site])	--exclude TCU 
			GROUP BY D.FiscalPeriodLong, CASE WHEN M.NewProgram is NULL THEN 'UNKNOWN' ELSE m.NewProgram END
		END

		--------------------------------------------------
		--Compute # of official cases by infection and nursing unit
		--------------------------------------------------
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_HAI_officialcases') IS NOT NULL DROP TABLE #VW_HAI_officialcases;
		END

		BEGIN
			SELECT I.[Period] as 'FiscalPeriodLong'
			, I.infection
			, CASE WHEN M.NewProgram is NULL THEN 'UNKNOWN' ELSE m.NewProgram END as 'Program'
			, SUM(cases) as 'NumCases'
			INTO #VW_HAI_officialcases
			FROM DSSI.dbo.RHMRSACDIPeriodVisibilityWall as I
			LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_NU_PROGRAM_MAP_ADTC]  as M
			ON I.NursingUnitCode=M.NursingUnitCode
			GROUP BY I.[Period]
			, I.infection
			, CASE WHEN M.NewProgram is NULL THEN 'UNKNOWN' ELSE m.NewProgram END
		END

		--------------------------------------------------
		--Combine all the results together
		--------------------------------------------------
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_HAI_combined') IS NOT NULL DROP TABLE #VW_HAI_combined;
		END

			BEGIN
			SELECT
			CASE WHEN X.infection='MRSA' THEN 28
				 WHEN X.infection='CDI' THEN 29
				 WHEN X.infection='CPO' THEN 30
				 ELSE NULL
			END as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, X.Infection
			, X.Program
			, X.FiscalPeriodEndDate as 'TimeFrame'
			, X.FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, X.Infection + ' Rate per 10,000 Patient Days' as 'IndicatorName'
			, CASE WHEN O.NumCases is not NULL THEN O.NumCases
				   WHEN O.NumCases is NULL AND U.NumCases is not NULL THEN U.NumCases
				   ELSE 0
			END as 'Numerator'
			, C.NumIPDays as 'Denominator'
			, 10000.0*CASE WHEN O.NumCases is not NULL THEN O.NumCases WHEN O.NumCases is NULL AND U.NumCases is not NULL THEN U.NumCases ELSE 0 END/ C.NumIPDays as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  CASE WHEN X.infection='MRSA' THEN 3.04
					WHEN X.infection='CDI' AND X.fiscalperiodlong<='2016-09' THEN 7.5
					WHEN X.infection='CDI' AND X.fiscalperiodlong >='2016-10' THEN 7.33
					WHEN X.infection='CPO' THEN NULL	--no known target
					ELSE NULL
			END as 'Target'
			, 'QPS' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_HAI_combined
			FROM #VW_HAI as X
			LEFT JOIN #VW_unofficialcases as U
			ON X.FiscalPeriodLong=U.FiscalPeriodLong AND X.Program=U.Program AND X.Infection=U.Infection
			LEFT JOIN #VW_HAI_officialcases as O
			ON X.FiscalPeriodLong=O.FiscalPeriodLong AND X.Program=O.Program AND X.Infection=O.Infection
			LEFT JOIN #VW_HAI_census as C
			ON X.FiscalPeriodLong=C.FiscalPeriodLong AND X.Program=C.Program
			--add overall
			UNION
			SELECT
			CASE WHEN X.infection='MRSA' THEN 28
				 WHEN X.infection='CDI' THEN 29
				 WHEN X.infection='CPO' THEN 30
				 ELSE NULL
			END as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, X.Infection
			, 'Overall' as 'Program'
			, X.FiscalPeriodEndDate as 'TimeFrame'
			, X.FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, X.Infection + ' Rate per 10,000 Patient Days' as 'IndicatorName'
			, SUM(CASE WHEN O.NumCases IS NOT NULL THEN O.NumCases
					   WHEN O.NumCases IS NULL AND U.NumCases IS NOT NULL THEN U.NumCases
					   ELSE 0
				  END
			) as 'Numerator'
			, SUM(C.NumIPDays) as 'Denominator'
			, 10000.0*SUM(CASE WHEN O.NumCases is not NULL THEN O.NumCases WHEN O.NumCases is NULL AND U.NumCases is not NULL THEN U.NumCases ELSE 0 END)/ SUM(C.NumIPDays) as 'Value'
			, 'Below' as 'DesiredDirection'
			, 'D1' as 'Format'
			,  CASE WHEN X.infection='MRSA' THEN 3.04
					WHEN X.infection='CDI' AND X.fiscalperiodlong<='2016-09' THEN 7.5
					WHEN X.infection='CDI' AND X.fiscalperiodlong >='2016-10' THEN 7.33
					WHEN X.infection='CPO' THEN NULL	--no known target
					ELSE NULL
			END as 'Target'
			, 'QPS' as 'DataSource'
			, 1 as 'IsOverall'
			FROM #VW_HAI as X
			LEFT JOIN #VW_unofficialcases as U
			ON X.FiscalPeriodLong=U.FiscalPeriodLong AND X.Program=U.Program AND X.Infection=U.Infection
			LEFT JOIN #VW_HAI_officialcases as O
			ON X.FiscalPeriodLong=O.FiscalPeriodLong AND X.Program=O.Program AND X.Infection=O.Infection
			LEFT JOIN #VW_HAI_census as C
			ON X.FiscalPeriodLong=C.FiscalPeriodLong AND X.Program=C.Program
			GROUP BY X.Infection, X.FiscalPeriodLong, X.FiscalPeriodEndDate
		END

	-----------------------------------------------
	-- ID31 Hand Hygiene Compliance Rates
	-----------------------------------------------
	/*
	Purpose: Identify the hand hygiene compliance rate
	Author: Hans Aisake
	Date Create: Sept 28, 2018
	Date Modified:
	Inclusions/exclusions: 
	Comments: We stopped recieving period data in 2018/2019-July because audting switched to quarterly.
	We have not gotten full HH quarterly data to replace it yet.
	
	*/

		--------------------------------------------------
		--	Reporting Periods
		--------------------------------------------------
		--find reporting periods current fiscal period and the last two fiscal years for the weekly date
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_HH_tf') IS NOT NULL DROP TABLE #VW_HH_tf;
		END

		BEGIN
			SELECT distinct TOP 36 fiscalperiodlong, fiscalperiodstartdate, fiscalperiodenddate
			INTO #VW_HH_tf
			FROM ADTCmart.dim.[Date]
			where fiscalperiodlong <= (select max([Month]) from DSSI.[dbo].[RHHandHygieneByUnitVisibilityWall] where facility='rh')
			ORDER BY fiscalperiodlong DESC
		END

		--compute the indicators
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_ID31') IS NOT NULL DROP TABLE #VW_ID31;
		END

		BEGIN
			SELECT 31 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			,case when H.unit in ('RH - 2 South','RH - 3 North','RH - 3 South','RH - Emergency','RH - ICU') then 'Critical Care-Med-Pat Flow'
				  when H.unit in ('RH - 4 North','RH - 6 North') then 'Med Adm-Surg-Amb'
				  when H.unit is null then 'Unknown'
				  else H.unit 
			end as 'Program'
			, D.FiscalPeriodEndDate as 'TimeFrame'
			, D.FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'Percentage of Hand Hygiene Compliance' as 'IndicatorName'
			, SUM(H.[completed]) as 'Numerator'
			, SUM(H.[total]) as 'Denominator'
			, 1.0*SUM(H.[completed])/SUM(H.[total]) as 'Value'
			, 'Above' as 'DesiredDirection'
			, 'D1' as 'Format'
			, 0.85 as 'Target'
			, 'QPS' as 'DataSource'
			, 0 as 'IsOverall'
			INTO #VW_ID31
			FROM #VW_HH_tf as D
			LEFT JOIN DSSI.[dbo].[RHHandHygieneByUnitVisibilityWall] as H
			ON D.FiscalPeriodLong=H.[Month]
			WHERE H.facility='rh'
			GROUP BY H.facility
			,case when H.unit in ('RH - 2 South','RH - 3 North','RH - 3 South','RH - Emergency','RH - ICU') then 'Critical Care-Med-Pat Flow'
				  when H.unit in ('RH - 4 North','RH - 6 North') then 'Med Adm-Surg-Amb'
				  when H.unit is null then 'Unknown'
				  else H.unit 
			end
			, D.FiscalPeriodLong
			, D.FiscalPeriodEndDate
			--add overall
			UNION
			SELECT 31 as 'IndicatorID'
			, 'Richmond' as 'Facility'
			, 'Overall' as 'Program'
			, D.FiscalPeriodEndDate as 'TimeFrame'
			, D.FiscalPeriodLong as 'TimeFrameLabel'
			, 'Fiscal Period' as 'TimeFrameType'
			, 'Percentage of Hand Hygiene Compliance' as 'IndicatorName'
			, SUM(H.[completed]) as 'Numerator'
			, SUM(H.[total]) as 'Denominator'
			, 1.0*SUM(H.[completed])/SUM(H.[total]) as 'Value'
			, 'Above' as 'DesiredDirection'
			, 'D1' as 'Format'
			, 0.85 as 'Target'
			, 'QPS' as 'DataSource'
			, 1 as 'IsOverall'
			FROM #VW_HH_tf as D
			LEFT JOIN DSSI.[dbo].[RHHandHygieneByUnitVisibilityWall] as H
			ON D.FiscalPeriodLong=H.[Month]
			WHERE H.facility='rh'
			GROUP BY H.facility
			, D.FiscalPeriodLong
			, D.FiscalPeriodEndDate
		END

	-----------------------------------------------
	-- Consolidate Indicators
	-----------------------------------------------
		/*	Purpose: Consolidate the Indicators for the Visibility Wall
			Author: Hans Aisake
			Date Created: September 24, 2018
			Date Modified: Jan 11, 2019
			Inclusions/Exclusions: 
			Comments:
		*/

		--CREATE TABLE DSSI.dbo.RH_VisibilityWall_IndicatorTableAll
		--( indicatorID varchar(15),
		--  facility varchar(255),
		--  entity_group varchar(255),
		--  TimeFrame date,	
		--  TimeFrameLabel varchar(20),	
		--  TimeFrameType varchar(50),
		--  IndicatorName varchar(255),
		--  Numerator int,
		--  Denominator int,
		--  Value float,
		--  DesiredDirection varchar(50),
		--  [Format] char(2),
		--  [Target] float,
		--  DataSource varchar(50),
		-- IsOverall int, 
		-- IndicatorClass varchar(50)
		--)

		BEGIN
			TRUNCATE TABLE DSSI.dbo.RH_VisibilityWall_IndicatorTableAll;
		END


		--insert values into the indicator table
		BEGIN
			INSERT INTO DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll] (indicatorID, Facility, entity_group,TimeFrame,TimeFrameLabel, TimeFrameType,IndicatorName,Numerator,Denominator,[Value],DesiredDirection,[Format],[Target],[DataSource], IsOverall)
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID01
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID02
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID03
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID04
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID05
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID06
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID07
			UNION
			SELECT IndicatorID, Waitlist , Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID09
			UNION
			SELECT IndicatorID, Waitlist, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID10
			UNION
			SELECT IndicatorID, Facility, LoggedMainsurgeonSpecialty, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID13
			UNION
			SELECT IndicatorID, Facility, Age_Group, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID14
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID15
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, timeFrameType, IndicatorName, Numerator, Denominator, [Value], Desireddirection, [Format], [Target], DataSource, IsOverall
			FROM #VW_ID16
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID17
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID18
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID19
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID20
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID21
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID22
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID23
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID24
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID25
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID26
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_HAI_combined
			UNION
			SELECT IndicatorID, Facility, Program, TimeFrame, TimeFrameLabel, TimeFrameType, IndicatorName, Numerator, Denominator, [Value], DesiredDirection, [Format], [Target], DataSource, IsOverall 
			FROM #VW_ID31
			ORDER BY IndicatorID ASC, Program ASC, TimeFrame ASC
		END

	------------
	-- DELETIONS
	------------

		--CASE not being tracked after 2019-01 at the moment
		BEGIN
			DELETE 
			FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
			WHERE indicatorName in ('Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)','UTI + Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)','UTI Rate per 1,000 Discharges (55+ medical/surgical)')
			AND timeframelabel >='2019-01'
		END

		--CASE data for these programs is not good enough to report on
		BEGIN
			DELETE 
			FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
			WHERE indicatorName in ('Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)','UTI + Pneumonia Rate per 1,000 Discharges (55+ medical/surgical)','UTI Rate per 1,000 Discharges (55+ medical/surgical)')
			AND entity_group in ('Mental Health & Addictions Ser','Pop & Family Hlth & Primary Cr','Unknown')
		END

		--MRSA/CDI/CPO data for these programs is not good enoguh to report on with SPC charts as is. T or G charts would be better. the events are too rare.
		BEGIN
			DELETE 
			FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
			WHERE indicatorName in ('CDI Rate per 10,000 Patient Days','CPO Rate per 10,000 Patient Days','MRSA Rate per 10,000 Patient Days')
			AND entity_group in ('Home & Community Care','Mental Health & Addictions Ser','Pop & Family Hlth & Primary Cr')
		END

		--delete CPO breakdowns due to lack of data
		BEGIN
			DELETE 
			FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
			WHERE indicatorName in ('CPO Rate per 10,000 Patient Days')
			AND entity_group !='Overall'
		END

		--Falls data for these programs is not good enough to report on with SPC charts as is. T or G charts would be better. the events are too rare.
		BEGIN
			DELETE 
			FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
			WHERE indicatorName in ('Falls Cases (Harm)','Falls Cases (No Harm)','Falls Cases (Total)')
			AND entity_group in ('Other','Pop & Family Hlth & Primary Cr','Mental Health & Addictions Ser')
		END

		BEGIN
			DELETE 
			FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
			WHERE indicatorName in ('Falls Cases (Harm)','Falls Cases (No Harm)')
			AND entity_group in ('Unknown', 'Mental Health & Addictions Ser','Pop & Family Hlth & Primary Cr','Home & Community Care')
		END

	--for indicators these indicators with very sparse records delete the entire indicator from the table
	--the values are seen as not being complete enough to be worth reporting, and furthermore cannot feed SPC charts appropriately
	--Although, the overall indicators will still include these cases.
	BEGIN
		DELETE X
		FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]  as X
		INNER JOIN 
		(
			SELECT * FROM
			(
				SELECT facility
				, entity_group
				, IndicatorName
				, indicatorID
				, SUM(CASE WHEN Value is not NULL THEN 1 ELSE 0 END) as 'Actual'
				, CASE WHEN TimeFrameType='Weekly' THEN  DATEDIFF(week,MIN(TimeFrame),MAX(TimeFrame))+1
					   WHEN TimeFrameType='Fiscal Period' THEN DATEDIFF(month, Min(timeFrame),Max(TimeFrame))+1
					   ELSE NULL
				END as 'Expected' 
				FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
				WHERE IndicatorName in ('Percent of ED Patients Admitted to Hospital Within 10 Hours','% of Face-to-Face Nursing Visits that were Ambulatory','% Placements within Target(30days) from VCH Acute','% Placements within Target (30days) from RH Community','% Surgical Patients Treated Within Target Wait Time','Short Stay Discharges (LOS<=48hrs)','ALC Rate Based on Discharges')
				GROUP BY facility
				, IndicatorName
				, indicatorID
				,TimeFrameType
				, entity_group
			) as y
			WHERE Actual <Expected*0.8	--based on the time stamps figure out how many rows there should be. If 80% or more aren't present delete the indicator
		) as Z
		ON X.facility=Z.facility
		AND X.indicatorID=Z.IndicatorID
		AND X.entity_group=Z.entity_group
	END

	--delete indicator with less than 4 values
	BEGIN
		DELETE X
		FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll] as X
		INNER JOIN (
		SELECT indicatorID, facility, entity_group, COUNT(*) as 'Count'
		FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
		GROUP BY indicatorID, facility, entity_group
		HAVING COUNT(*) <4
		) as Y
		ON X.indicatorID=Y.indicatorID
		AND X.Facility=Y.facility
		AND X.Entity_Group=Y.entity_group
	END


	BEGIN
		--remove these charts because the data is highly irrelevant for the context
		DELETE
		FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
		WHERE indicatorName in ('Days >30 for Discharged Patients','Current Inpatient Long Length of Stay (LLOS) Inpatient Days*','Average ALC (all types) Census','ALC Rate Based on Discharges')
		and entity_group in ('Population & Family Health','Pop & Family Hlth & Primary Cr')

		--undersireable program breakdown
		DELETE FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
		WHERE indicatorName  in ('Inpatient Census','% Surgical Patients Treated Within Target Wait Time') 
		AND entity_group='Unknown'
	
		--insufficient volume for statistical analysis
		DELETE FROM DSSI.[dbo].[RH_VisibilityWall_IndicatorTableAll]
		WHERE indicatorName in ('Average ALC (all types) Census')
		AND entity_group='Home & Community Care'
	END
-----------------------------------------------
-- Supporting tables for non-indicator charts
-----------------------------------------------

	-----------------------------------------------
	-- ID11 Residential Care Waitlist AAP
	-----------------------------------------------
	/*
	Purpose: Show snapshots of the waitlist length at various points in time
	Author: Hans Aisake
	Date Created: June 19, 2018
	Date Modified: Oct 19, 2018
	Inclusions/Exclusions: 
	-Includes palcements from the Richmond Priority Access Wait List only.
	Comments:
	*/
	
	--identify suspensions for relvant waitlist clients
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_susData11') IS NOT NULL DROP TABLE #VW_susData11
	END

	BEGIN
		SELECT SUS.sourcewaitlistEntryID
		, CONVERT(date,CONVERT(varchar(8), StartDateID),112) as 'StartDate'
		, CASE  WHEN EndDateID is NULL and W.DateOffWaitlist is NULL THEN '2050-01-01'
				WHEN EndDateID is not NULL AND W.DateOffwaitlist is NULL THEN CONVERT(date,CONVERT(varchar(8), EndDateID),112)
				WHEN EndDateID is NULL AND W.DateOffWaitlist is not NULL THEN W.DateOffWaitlist
				WHEN EndDateID Is not NULL and W.DateOffWaitlist is not NULL AND CONVERT(date,CONVERT(varchar(8), EndDateID),112)<=W.DateOffwaitlist THEN CONVERT(date,CONVERT(varchar(8), EndDateID),112)
				WHEN EndDateID Is not NULL and W.DateOffWaitlist is not NULL AND W.DateOffwaitlist<=CONVERT(date,CONVERT(varchar(8), EndDateID),112) THEN W.DateOffWaitlist
				ELSE NULL
		END as 'EndDate'
		INTO #VW_susData11
		FROM COmmunityMart.[dbo].[WaitlistSuspensionFact] as SUS
		INNER JOIN COmmunityMart.dbo.vwPARISWaitlist as W	--suspension corespondes to waitlist RH priority access
		ON W.WaitlistName ='PRIORITY ACCESS RICHMOND' 
		AND SUS.sourcewaitlistEntryID=W.SourceWaitlistEntryID
	END

	--pull waitlist data
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_rcdata11') IS NOT NULL DROP TABLE #VW_rcdata11
	END

	BEGIN
		SELECT DISTINCT			--there shouldn't be duplicates but we have a few DQ issues
		D.FiscalPeriodLong
		, D.FiscalPeriodEndDate
		, IIF(P.DeathDate >D.fiscalPeriodEndDate OR P.DeathDate is NULL, w.[SourceSystemClientID], NULL) as 'NotDeceased_SourceSystemClientID'
		, ISNULL(MAP.Custom_LocationCategory,'Unknown') as 'RegistrationFromLocationCategory'
		--, ISNULL(MAP.WaitTimeTarget,30) as 'Target'
		INTO #VW_rcdata11
		FROM (SELECT * FROM CommunityMart.[dbo].[vwPARISWaitlist] WHERE WaitlistName ='PRIORITY ACCESS RICHMOND') as W		--only pull waitlist data for the waitlists I care about
		LEFT JOIN CommunityMart.[dbo].[vwPARISPersonWithIdentifier] p		--join to identify if clients died by the date of interest
		ON p.[PatientID] = w.[PatientID]
		INNER JOIN (SELECT TOP 28 * FROM #VW_FPReportTF WHERE FiscalPeriodEndDate < (SELECT MAX(dateonwaitlist) FROM CommunityMart.dbo.vwPARISwaitlist WHERE waitlistname='PRIORITY ACCESS RICHMOND') ORDER BY fiscalPeriodEndDate DESC) as D					--identify which reporting time frame the data coresponds too
		ON D.FiscalPeriodEndDate>= W.DateOnWaitlist									--registered before thursday week end
		AND (D.FiscalPeriodEndDate <=W.DateOffWaitlist OR W.DateoffWaitlist is NULL)	--not placed before thursday week end
		LEFT JOIN #VW_susData11 as S
		ON W.SourceWaitlistEntryID=S.SourceWaitListEntryID						--find relevant suspension data
		AND D.FiscalPeriodEndDate BETWEEN S.StartDate AND S.EndDate					--identify if clients were suspended on the reporting time frame
		LEFT JOIN DSSI.[dbo].[RH_VisibilityWall_AF_RCLocationTypeMap] as MAP
		ON W.CurrentLocationType_DateOnWaitlist=MAP.cmart_currentlocationtype	--same location
		WHERE S.SourceWaitListEntryID is NULL									--exclude clients that were suspended on the reporting time frame
	END

	--clear out old results in the table
	BEGIN
		TRUNCATE TABLE DSSI.dbo.RH_VisibilityWall_AF_RCWaitLength
	END

	BEGIN
		--compute the waitlist length and store into the table
		INSERT INTO DSSI.dbo.RH_VisibilityWall_AF_RCWaitLength (TimeFrameLabel,TimeFrame, RegLocation, NumClientsAAP)
		SELECT FiscalPeriodLong as 'TimeFrameLabel'
		, CAST(FiscalPeriodEndDate as date) as 'TimeFrame'
		, RegistrationFromLocationCategory
		, COUNT(NotDeceased_SourceSystemClientID) as 'NumClientsAAP'
		FROM #VW_rcdata11
		GROUP BY FiscalPeriodLong
		, FiscalPeriodEndDate
		, RegistrationFromLocationCategory
	END

	-----------------------------------------------
	-- ID12 Residential Care Waitlist AAP Current days Waiting Distribution
	-----------------------------------------------
	/*
	Purpose: Identify how many people have been waiting X days at the end of each thursday week
	Author: Hans Aisake
	Date Created: May 10, 2018
	Date Modified: June 19, 2018
	Inclusions/exclusions:
	Comments:
	*/

	---------------------------------------------
	--Create a placeholder table to fill in 0s for buckets
	---------------------------------------------
	--identify suspensions for relvant waitlist clients
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_susData12') IS NOT NULL DROP TABLE #VW_susData12
	END

	BEGIN
		SELECT SUS.sourcewaitlistEntryID
		, CONVERT(date,CONVERT(varchar(8), StartDateID),112) as 'StartDate'
		, CASE  WHEN EndDateID is NULL and W.DateOffWaitlist is NULL THEN '2050-01-01'
				WHEN EndDateID is not NULL AND W.DateOffwaitlist is NULL THEN CONVERT(date,CONVERT(varchar(8), EndDateID),112)
				WHEN EndDateID is NULL AND W.DateOffWaitlist is not NULL THEN W.DateOffWaitlist
				WHEN EndDateID Is not NULL and W.DateOffWaitlist is not NULL AND CONVERT(date,CONVERT(varchar(8), EndDateID),112)<=W.DateOffwaitlist THEN CONVERT(date,CONVERT(varchar(8), EndDateID),112)
				WHEN EndDateID Is not NULL and W.DateOffWaitlist is not NULL AND W.DateOffwaitlist<=CONVERT(date,CONVERT(varchar(8), EndDateID),112) THEN W.DateOffWaitlist
				ELSE NULL
		END as 'EndDate'
		INTO #VW_susData12
		FROM COmmunityMart.[dbo].[WaitlistSuspensionFact] as SUS
		INNER JOIN COmmunityMart.dbo.vwPARISWaitlist as W	--suspension corespondes to waitlist RH priority access
		ON W.WaitlistName ='PRIORITY ACCESS RICHMOND' 
		AND SUS.sourcewaitlistEntryID=W.SourceWaitlistEntryID
	END

	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_susDays12') IS NOT NULL DROP TABLE #VW_susDays12
	END

	BEGIN
		SELECT D.ThursdayWkEnd
		, s.SourceWaitListEntryID
		--identify how many days from the suspension start to thursday week end there were for each waitlist, suspension record combo
		--if the suspension goes past the thursday week only one take the suspension proportion up to the thursday week end
		--otherwise if the suspension ends before the thursday week end take all the suspension days
		, DATEDIFF(day, S.StartDate, CASE WHEN S.EndDate<=D.ThursdayWkEnd THEN S.EndDate ELSE D.ThursdayWkEnd END) as 'CumulativeSuspendedDays' 
		INTO #VW_susDays12
		FROM #VW_weekReportTF as D
		LEFT JOIN #VW_susData12 as S
		ON S.StartDate <= D.ThursdayWkEnd
	END
	
	--pull relevant waitlist data
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#VW_wlData12') IS NOT NULL DROP TABLE #VW_wlData12
	END

	BEGIN
		SELECT DISTINCT WL.SourceSystemClientID
		, D.ThursdayWkEnd
		, DATEDIFF(day, WL.DateOnWaitList, D.thursdayWkEnd) - ISNULL(CumulativeSuspendedDays,0) as 'WaitTime'
		INTO #VW_wlData12
		FROM CommunityMart.dbo.vwPARISWaitlist as WL
		INNER JOIN #VW_weekReportTF as D
		ON WL.DateOnWaitlist <= D.ThursdayWkEnd --was actively waiting on the fiscal period end date
		AND (WL.DateOffWaitlist>=ThursdayWkEnd OR WL.DateOffWaitlist is NULL)
		LEFT JOIN #VW_susDays12 as SD
		ON WL.SourceWaitlistEntryID=SD.SourceWaitListEntryID
		AND D.ThursdayWkEnd=SD.ThursdayWkEnd
		WHERE WL.WaitlistName='PRIORITY ACCESS RICHMOND'
	END

	--clear out old values in the table for a full update
	BEGIN
		TRUNCATE TABLE DSSI.dbo.RH_VisibilityWall_AF_RCCurrWait
	END

	--insert new values into the table. Creates a palce holder with all buckets and weeks so 0's are properly captured and data is compelte
	BEGIN
		INSERT INTO DSSI.dbo.RH_VisibilityWall_AF_RCCurrWait (ThursdayWkEnd,Buckets, [Order], [Count])
		SELECT T.ThursdayWkEnd
		, Y.Buckets
		, Y.[Order]
		, SUM(CASE WHEN WL.SourceSystemClientID is not NULL THEN 1 ELSE 0 END) as 'Count' 
		FROM (SELECT * FROM #VW_weekReportTF) as T
		CROSS JOIN (SELECT * FROM DSSI.dbo.RH_VisibilityWall_AF_Buckets) Y
		LEFT JOIN #VW_wlData12 as WL
		ON T.ThursdayWkEnd=WL.ThursdayWkEnd
		AND WL.WaitTime BETWEEN Y.BucketStart AND Y.BucketEnd
		GROUP BY T.ThursdayWkEnd
		, Y.Buckets
		, Y.[Order]
	END

	--------------------------
	-- RC Placements
	--------------------------
	--see construction of #linkedData for details
	BEGIN
		TRUNCATE TABLE DSSI.dbo.RH_VisibilityWall_AF_RCPlacements
	END

	BEGIN
		INSERT INTO DSSI.dbo.RH_VisibilityWall_AF_RCPlacements
		SELECT D.FiscalPeriodLong
		, PlacedFromLocation
		, COUNT(distinct PatientID) as 'NumPlacements'
		FROM #VW_rc_adtcLinkedData as X
		INNER JOIN (SELECT TOP 26 * FROM #VW_FPReportTF ORDER BY FiscalPeriodEndDate DESC) as D
		ON X.DateOffWaitList BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
		GROUP BY D.FiscalPeriodLong
		, PlacedFromLocation
	END

	-----------------------------
	--OR Waitlist
	-----------------------------
	--clear out old data in the results table
	BEGIN
		TRUNCATE TABLE DSSI.dbo.RH_VisibilityWall_AF_ORWaitLength
	END

	--identify the max OR date in the ORMart data set for informational purposes and cutoff logics
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#maxORDate_VW_AF') is NOT NULL DROP TABLE #maxORDate_VW_AF
	END

	BEGIN
		SELECT max([ExtractFileDate]) as MaxORWaitListDate
		INTO #maxORDate_VW_AF
		FROM ORMart.[dbo].[vwRegionalORWaitlistCase] as O
		WHERE facilitylongname='richmond hospital'
		AND [IsSPRCase]=1
		AND DxTargetInWeeks is not null
		AND [BookingFormReceivedDate] is not null
		AND [ScheduledSPRPx1Code] not in ('11048','12001','12002','12003','12004','12005','12006','12007','12008','12009','20135','30007','40018','40033','10161','10163','20123','20124','40029','20012','20049','20138','40040')
	END

	--find number of OR cases by OR service and within or outside of target
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#ORcounts_VW_AF') is NOT NULL DROP TABLE #ORcounts_VW_AF
	END

	BEGIN
		SELECT [ServiceDescription] as 'ORService'
		, 'Within Target' as 'WithinDxTargetFlag'
		, COUNT(1) as 'NumCases'
		, CAST((SELECT MaxORWaitListdate FROM #maxORDate_VW_AF) as date) as 'MaxORDate'
		INTO #ORcounts_VW_AF
		FROM ORMArt.[dbo].[vwRegionalORWaitlistCase]
		WHERE facilitylongname='richmond hospital'
		AND [IsSPRCase]=1
		AND DxTargetInWeeks is not null
		AND [BookingFormReceivedDate] is not null
		AND [ScheduledSPRPx1Code] not in ('11048','12001','12002','12003','12004','12005','12006','12007','12008','12009','20135','30007','40018','40033','10161','10163','20123','20124','40029','20012','20049','20138','40040')
		AND datediff(wk,[BookingFormReceivedDate], (SELECT * FROM #maxORDate_VW_AF)) <= CAST(DxTargetInWeeks as float)
		GROUP BY ServiceDescription
		UNION
		SELECT [ServiceDescription] as 'ORService'
		, 'Over Target' as 'WithinDxTargetFlag'
		, COUNT(1) as 'NumCases'
		, CAST((SELECT MaxORWaitListdate FROM #maxORDate_VW_AF) as date) as 'MaxORDate'
		FROM ORMArt.[dbo].[vwRegionalORWaitlistCase]
		WHERE facilitylongname='richmond hospital'
		AND [IsSPRCase]=1
		AND DxTargetInWeeks is not null
		AND [BookingFormReceivedDate] is not null
		AND [ScheduledSPRPx1Code] not in ('11048','12001','12002','12003','12004','12005','12006','12007','12008','12009','20135','30007','40018','40033','10161','10163','20123','20124','40029','20012','20049','20138','40040')
		AND datediff(wk,[BookingFormReceivedDate], (SELECT * FROM #maxORDate_VW_AF)) > CAST(DxTargetInWeeks as float)
		GROUP BY ServiceDescription
	END

	--find the total number of cases by OR service and within target flag description
	BEGIN
		IF OBJECT_ID('tempdb.dbo.#ORToTcounts_VW_AF') is NOT NULL DROP TABLE #ORToTcounts_VW_AF
	END

	BEGIN
		SELECT ORService, SUM(NumCases) as 'TotalCases'
		INTO #ORToTcounts_VW_AF
		FROM #ORcounts_VW_AF
		GROUP BY ORService
	END

	--attach the % labels and the case counts and insert the data into a table in DSSI.
	BEGIN
		INSERT INTO DSSI.dbo.RH_VisibilityWall_AF_ORWaitLength (ORService,WithinDxTargetFlag,NumCases,MaxORDate, PercentCases,PercentLabel)
		SELECT X.ORService
		, X.WithinDxTargetFlag
		, X.NumCases
		, X.MaxORDate
		, ROUND(1.0*X.NumCases/Y.TotalCases,3) as 'PercentCases'
		, LEFT(CONVERT (VARCHAR(50), ROUND(100.0*X.NumCases/Y.TotalCases, 1),128),4) as 'PercentLabel'
		FROM #ORcounts_VW_AF as X
		LEFT JOIN #ORToTcounts_VW_AF as Y
		ON X.ORService=Y.ORService
	END


	-----------------------------------------------
	-- Falls by Age Group ID27
	-----------------------------------------------
	/*
	Purpose: Pull Number of Falls for the visibility wall reports by age group
	Author: Peter Kaloupis
	Co-author: Hans Aisake
	Date Created: 2015 
	Date Modified: 
	Comments: only overall programs
	*/

		--identify the time frames for the falls. Data comes by month and year, so I'm going to report it by month and year. Mapping it to FP is disingenuous. Shorter than _tf1
		BEGIN
			IF OBJECT_ID('tempdb.dbo.#VW_falls_tf2') IS NOT NULL DROP TABLE #VW_falls_tf2	
		END

		BEGIN
			SELECT distinct TOP 26 [Year], [Month]
			INTO #VW_falls_tf2
			FROM [DSSI].[dbo].[RHFallsVisibilityWall]
			ORDER BY [Year] DESC, [Month] DESC
		END

		--clear the staging table
		BEGIN
			TRUNCATE TABLE [DSSI].[dbo].[RH_VisibilityWall_QPS_FallsByAge];
		END

		--load data into a staging table for the report
		BEGIN
			INSERT INTO [DSSI].[dbo].[RH_VisibilityWall_QPS_FallsByAge] (AgeGroupName,[Year], [Month],TimeFrameLabel, Program,NumFallsCases)
			SELECT F.[Age Group Name] as 'AgeGroupName'
			, F.[year]
			, F.[month]
			, CAST(F.[Year] as varchar(4)) + '-' + CASE WHEN LEN(F.[Month])=1 THEN '0' + CAST(F.[Month] as varchar(1)) ELSE CAST(F.[Month] as varchar(2)) END as 'TimeFrameLabel'
			, 'Overall' as 'Program'
			, COUNT(*) as 'NumFallsCases'
			FROM [DSSI].[dbo].[RHFallsVisibilityWall]  as F
			INNER JOIN #VW_falls_tf2 as D
			ON F.[Year]=D.[Year] AND F.[Month] = D.[Month]
			GROUP BY  F.[Age Group Name]
			, F.[year]
			, F.[month]
			, CAST(F.[Year] as varchar(4)) + '-' + CASE WHEN LEN(F.[Month])=1 THEN '0' + CAST(F.[Month] as varchar(1)) ELSE CAST(F.[Month] as varchar(2)) END
		END

---------------------------------------------
--update indicator names in auxillary tables
---------------------------------------------
	
	--ADDING NEW INDICATORS
	--To add new indicators build section ID(X+1) above then add the following code to the bottom of the above.
	--UNION
	--SELECT X+1 as 'IndicatorID', ......
	--FROM #VW_ID(X+1)

	--Then add chart type for the new indicator to DSSI.[dbo].[RH_VisibilityWall_AF_IndicatorChartType]
	--INSERT INTO DSSI.[dbo].[RH_VisibilityWall_AF_IndicatorChartType] (indicatorID,facility,indicatorName,entity_group,ChartType)
	--SELECT distinct indicatorId, facility, indicatorName, entity_group, 'i' 
	--FROM DSSI.dbo.RH_VisibilityWall_AF_IndicatorTableAll WHERE indicatorID=(x+1)

	--Then add filters to  [DSSI].[dbo].[RH_VisibilityWall_AF_IndicatorChartFilters]
	--INSERT INTO [DSSI].[dbo].[RH_VisibilityWall_AF_IndicatorChartFilters](indicatorID,facility,indicatorName,entity_group,FirstDate)
	--SELECT distinct indicatorId, facility, indicatorName, entity_group, 'YYYY-MM-DD' 
	--FROM DSSI.dbo.RH_VisibilityWall_AF_IndicatorTableAll WHERE indicatorID=(x+1)
	--the first date is based on when you want to start history for the baseline computations; select the latest stable representative period that makes sense. But you should have at least a dozen data points.

---------------------------------------------
--update indicator names in auxillary tables
---------------------------------------------
	--UPDATE [DSSI].[dbo].[RH_VisibilityWall_AF_IndicatorChartFilters]
	--SET indicatorName=Y.indicatorName
	--FROM [DSSI].[dbo].[RH_VisibilityWall_AF_IndicatorChartFilters] as X
	--LEFT JOIN (SELECT distinct indicatorID, indicatorName FROM DSSI.dbo.RH_VisibilityWall_AF_IndicatorTableAll) as Y
	--ON X.indicatorID=Y.indicatorID
	--;

	--UPDATE DSSI.[dbo].[RH_VisibilityWall_AF_IndicatorChartType]
	--SET indicatorName=Y.indicatorName
	--FROM DSSI.[dbo].[RH_VisibilityWall_AF_IndicatorChartType] as X
	--LEFT JOIN (SELECT distinct indicatorID, indicatorName FROM DSSI.dbo.RH_VisibilityWall_AF_IndicatorTableAll) as Y
	--ON X.indicatorID=Y.indicatorID
	--;

------------------
--create pattern exception table
-------------------
--CREATE TABLE DSSI.[dbo].[RH_VisibilityWall_InvestigatedPatterns](
--	indicatorID varchar(15),
--	indicatorName varchar(255),
--	entity_group varchar(255),
--	facility varchar(255),
--	pattern_start varchar(20), --timeframelabel
--	pattern_end varchar(20), --timeframelabel
--	PType varchar(50),
--)


------------
-- END QUERY
------------

END

