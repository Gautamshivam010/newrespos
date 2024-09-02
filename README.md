# newrespos
    
--use DACSReporting
IF OBJECT_ID('csp_NewYorkPASGetClientTransferDetails', 'P') IS NOT NULL

BEGIN
	DROP PROCEDURE [dbo].[csp_NewYorkPASGetClientTransferDetails]
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE csp_NewYorkPASGetClientTransferDetails 
(  
@StartDate DATE,
@EndDate DATE,
@ProgramId VARCHAR(MAX),
@ProgramReportingUnit VARCHAR(MAX)
)  
AS 

/*********************************************************************                                                                                            
File Procedure: dbo.[csp_NewYorkPASGetClientTransferDetails]  
  
Customer: Streamline 
  
Input Parameters:@StartDate  DATE, @EndDate  DATE 
Output Parameter:  Exec csp_NewYorkPASGetClientTransferDetails '2005-10-22','2021-10-25'
  
Called By: 
  
**  Date:         Author:            Description:  
  --------        --------           -------------------------------------------      
** 06 Oct 2019   Sasikumar C         What:To Get the list of PAS47 Client Transfer from one Program to another Program details.  
                                     Why :This will extract data for the Client's Transfer to another Program and to
									      display on the report and include in a file submission to the state . 
									      As per task  Villa of home customization#46.  
**02 Feb 2022  Senthil				What/Why : "ProviderClientID" is Corrected as per logic, Villa of Hope EIT (Environment Issue Tracking), 44 						                                           
**10 Feb 2022  Senthil			    What/Why : "ProviderClientID" is Corrected as per logic - Core Bugs, 5810
**09/8/2024	   Richa A			    What/why:Made Added Programs and ProgramReportingUnit Filters to the report as part of Engineering Improvement Initiatives- NBL(I), 128087
*********************************************************************/   
BEGIN  
     BEGIN TRY  

           /********************************************************************* 
                     Local Variable Declaration
            *********************************************************************/    	 
            DECLARE @ProviderNumber VARCHAR(MAX)
				    ,@DACS_InProgress_RecordStatusGCId INT
				    ,@PASBatchBatchTypeId INT  
					,@AcceptedRecordStatusGCId INT
					,@StartDate1 DATE
					,@EndDate1 DATE
			SET @ProviderNumber = (SELECT TOP 1 [Value] FROM SystemConfigurationKeys WHERE [Key] ='XSetOASASProviderNumber' AND ISNULL(RecordDeleted,'N')='N')
			SET @DACS_InProgress_RecordStatusGCId = (SELECT TOP 1 GlobalCodeId FROM GlobalCodes WHERE Category='DACSRecordStatus' AND Code='INPROGRESS' AND [Active]='Y' AND ISNULL(RecordDeleted,'N')='N')	
			SET @PASBatchBatchTypeId = (SELECT TOP 1 DACSBatchTypeId FROM DACSBatchTypes WHERE BatchName = 'New York PAS Batch' AND ISNULL(RecordDeleted,'N')='N')   
			SET @AcceptedRecordStatusGCId = (SELECT TOP 1 GlobalCodeId FROM GlobalCodes WHERE Category='DACSRecordStatus' AND Code='ACCEPTED' AND [Active]='Y' AND ISNULL(RecordDeleted,'N')='N')   
			SET @StartDate1 =@StartDate
			SET @EndDate1 = @EndDate

			/************************************************************************/
			DECLARE @IsAllProgramReportingUnit INT = 0
			DECLARE @ProgramReportingUnits TABLE (ProgramReportingUnitId INT)
			INSERT INTO @ProgramReportingUnits
			SELECT * FROM dbo.fnSplit(@ProgramReportingUnit, ',')
			
			IF EXISTS (SELECT 1 FROM @ProgramReportingUnits WHERE ProgramReportingUnitId = '-1')
			BEGIN
			    SET @IsAllProgramReportingUnit = 1
			END

			/************************************************************************/
			DECLARE @IsAllProgram INT = 0
			DECLARE @ProgramDetails TABLE (ProgramId INT)
			INSERT INTO @ProgramDetails
			SELECT * FROM dbo.fnSplit(@ProgramId, ',')
			
			IF EXISTS (SELECT 1 FROM @ProgramDetails WHERE ProgramId = '-1')
			BEGIN
			    SET @IsAllProgram = 1
			END

           /********************************************************************* 
                   Temp table declaration to hold data 
           *********************************************************************/    
           IF OBJECT_ID('tempdb..#ClienttransferfromProgram') IS NOT NULL  
           BEGIN  
			 DROP TABLE #ClienttransferfromProgram
           END 
           CREATE TABLE #ClienttransferfromProgram  
		   (  
           ClientId INT NULL,
           ClientInpatientVisitId INT NULL, 
           BedAssignmentId INT NULL,
	       StartDate DATETIME NULL,
	       EndDate DATETIME NULL,
	       Disposition INT NULL,
           ProgramName VARCHAR(500)NULL,
           ProgramId INT NULL,
	       Programcode VARCHAR(50)NULL,
           Rownum INT NULL,
	       CodeName VARCHAR(50)NULL
		  )

	      IF OBJECT_ID('tempdb..#ClienttransfertoProgram') IS NOT NULL  
          BEGIN  
			DROP TABLE #ClienttransfertoProgram
          END
          CREATE TABLE #ClienttransfertoProgram 
		  (
          ClientId INT NULL,
          ClientInpatientVisitId INT NULL, 
          BedAssignmentId INT NULL,
	      StartDate DATETIME NULL,
	      EndDate DATETIME NULL,
	      Disposition INT NULL,
          ProgramName VARCHAR(500)NULL,
          ProgramId INT NULL,
	      Programcode VARCHAR(50)NULL,
          Rownum INT NULL,
	      CodeName VARCHAR(50)NULL
		   )

		  IF OBJECT_ID('tempdb..#ClientTransferProgramdetails') IS NOT NULL  
          BEGIN  
            DROP TABLE #ClientTransferProgramdetails  
          END  
          CREATE TABLE #ClientTransferProgramdetails  
          (  
          ClientIdentifier INT NULL,
          SourceRecordIdentifier INT NULL, 
          UniqueRecordIdentifier INT NULL,
	      TransferfromProgramNo INT NULL,
	      TransferToProgramNo INT NULL,
	      Transferdate VARCHAR(10) NULL,
	      Transferdatetime DATETIME NULL,
          )

		   /********************************************************************* 
                 Temp table For #MostRecentPAS44Document
         *********************************************************************/ 

		 IF OBJECT_ID('tempdb..#MostRecentPAS44Document') IS NOT NULL
		 BEGIN
		    DROP TABLE #MostRecentPAS44Document
		 END
		 CREATE TABLE #MostRecentPAS44Document
		 (
			 ClientId INT
			,DocumentId INT
			,DocumentVersionId INT
			,EffectiveDate DateTime
			,GenderAtBirth INT
			,DateOfBirth DateTime
			,SSN VARCHAR(25)
			,LastName VARCHAR(50)
			,TEDSEpisodeId INT
			)



		  
         IF OBJECT_ID('tempdb..#ClientTransfersigneddocuments') IS NOT NULL  
         BEGIN  
			DROP TABLE #ClientTransfersigneddocuments  
         END 
         CREATE TABLE #ClientTransfersigneddocuments  
         (  
         DocumentId INT NULL, 
         ClientId INT NULL,
         DocumentVersionId INT NULL 
         )
		 
        

          /************************************************************************************ 
                 Get the client and their Visit information Which has transfered From  Program
          ************************************************************************************/  

             INSERT INTO #ClienttransferfromProgram
             SELECT Distinct CIV.ClientId
			 ,CIV.ClientInpatientVisitId
			 ,BA.BedAssignmentId
			 ,BA.StartDate
			 ,BA.EndDate
			 ,BA.Disposition
			 ,GCCP.CodeName AS ProgramName
			 ,BA.ProgramId
			 ,GCCP.ExternalCode1 AS ProgramFromName,
             ROW_NUMBER() OVER (PARTITION BY CIV.ClientId ORDER BY BA.BedAssignmentId DESC) AS rownum 
			 ,GCTF.CodeName
             FROM ClientInpatientVisits CIV 
             JOIN BedAssignments BA ON BA.ClientInpatientVisitId=CIV.ClientInpatientVisitId
             JOIN Programs P ON P.ProgramId=BA.ProgramId
             JOIN CustomPrograms CP ON CP.ProgramId=P.ProgramId
             LEFT JOIN GlobalCodes GCTF ON GCTF.GlobalCodeId=BA.Disposition AND GCTF.Category='BEDASSIGNDISPOSITION' 
             AND ISNULL(GCTF.RecordDeleted,'N')='N' AND ISNULL(GCTF.[Active],'Y')='Y' 
             LEFT JOIN GlobalCodes GCCP ON GCCP.GlobalCodeId=CP.OASASProgramCategoryId AND GCCP.Category='XOASASPrograms' 
             AND ISNULL(GCCP.RecordDeleted,'N')='N' AND ISNULL(GCCP.[Active],'Y')='Y'
             WHERE GCTF.Code='Transferred'
             AND GCCP.CodeName IS NOT NULL
			 AND (@IsAllProgram = 1 OR GCCP.GlobalCodeId IN (SELECT ProgramId FROM @ProgramDetails))


			   
          /************************************************************************************ 
                 Get the client and their Visit information Which has transfered To  Program
          ************************************************************************************/  

               INSERT INTO #ClienttransfertoProgram
               SELECT Distinct CIV.ClientId
			   ,CIV.ClientInpatientVisitId
			   ,BA.BedAssignmentId
			   ,BA.StartDate
			   ,BA.EndDate
			   ,BA.Disposition
			   ,GCCP.CodeName AS ProgramName
			   ,BA.ProgramId
               ,GCCP.ExternalCode1 as ProgramTOName
               ,ROW_NUMBER() OVER (PARTITION BY CIV.ClientId ORDER BY BA.BedAssignmentId DESC) AS rownum
			   ,GCTF.CodeName 
               FROM ClientInpatientVisits CIV 
               JOIN BedAssignments BA ON BA.ClientInpatientVisitId=CIV.ClientInpatientVisitId
               JOIN Programs P ON P.ProgramId=BA.ProgramId
               JOIN CustomPrograms CP ON CP.ProgramId=P.ProgramId
               LEFT JOIN GlobalCodes GCTF ON GCTF.GlobalCodeId=BA.Disposition AND GCTF.Category='BEDASSIGNDISPOSITION' 
               AND ISNULL(GCTF.RecordDeleted,'N')='N' AND ISNULL(GCTF.[Active],'Y')='Y' 
               LEFT JOIN GlobalCodes GCCP ON GCCP.GlobalCodeId=CP.OASASProgramCategoryId AND GCCP.Category='XOASASPrograms' 
               AND ISNULL(GCCP.RecordDeleted,'N')='N' AND ISNULL(GCCP.[Active],'Y')='Y'
               WHERE ISNULL(GCTF.Code,'')<>'Transferred' 
               AND EXISTS (SELECT ClientId FROM #clienttransferfromProgram WHERE ClientId=CIV.ClientId AND rownum=1)
			   AND (@IsAllProgram = 1 OR GCCP.GlobalCodeId IN (SELECT ProgramId FROM @ProgramDetails))

          /************************************************************************************ 
                 Get the client and their Visit information Which has transfered from one Program to another Program
          ************************************************************************************/  

               INSERT INTO #ClientTransferProgramdetails
               SELECT Distinct t.ClientId AS ClientIdentifier
			   ,t.ClientInpatientVisitId AS SourceRecordIdentifier
			   ,t.BedAssignmentId AS UniqueRecordIdentifier
			   ,t.Programcode AS TransferfromProgramNo
			   ,t1.Programcode AS TransferToProgramNo
			   ,FORMAT(t1.StartDate, 'MMddyyyy') AS Transferdate
               ,t1.StartDate AS [Transferdatetime]
               FROM #ClienttransferfromProgram t
               JOIN #ClienttransfertoProgram t1 ON t.ClientId=t1.ClientId 
               WHERE t.rownum=1 and t1.rownum=1 and t1.[StartDate] BETWEEN @StartDate1 AND @EndDate1
               AND t.Programcode<>t1.Programcode   
			   
			      
          /************************************************************************************ 
                 Get the Data of MostRecentPAS44Document
          ************************************************************************************/  
			  INSERT INTO #MostRecentPAS44Document
			  SELECT 
			  A.ClientId
			 ,A.DocumentId 
			 ,A.DocumentVersionId 
			 ,A.EffectiveDate 
			 ,A.GenderAtBirth 
			 ,A.DateOfBirth 
			 ,A.SSN 
			 ,A.LastName
			 ,A.TEDSEpisodeId
			  FROM (SELECT 
							 D.ClientId
							,D.DocumentId
							,PG.DocumentVersionId
							,D.EffectiveDate
							,PG.GenderAtBirth 
							,PG.DateOfBirth 
							,PG.SSN 
							,PG.LastName
							,PG.TEDSEpisodeId
							,ROW_NUMBER()OVER(PARTITION BY D.ClientId ORDER BY D.Effectivedate DESC,D.Modifieddate DESC) AS RowNumber                 
							 FROM #ClientTransferProgramdetails RC 
							 JOIN Documents D ON RC.ClientId=D.ClientId 
							 JOIN CustomDocumentPAS44Generals PG ON PG.DocumentVersionId=D.CurrentDocumentVersionId
							 WHERE D.[Status] = 22 
							 AND D.DocumentCodeId = @PAS44_DoCodeId
							 AND (DATEDIFF(DAY, D.EffectiveDate,@StartDate1)<=0)  
							 AND (DATEDIFF(DAY, D.EffectiveDate,@EndDate1) >= 0)
							 AND ISNULL(D.RecordDeleted,'N')='N'  
							 AND ISNULL(PG.RecordDeleted,'N')='N'  
				) AS A
				WHERE A.RowNumber=1           

			   
          /************************************************************************************ 
                 Get the client and their NYPASReporting Signed documents based on input date range
          ************************************************************************************/  

               INSERT INTO #ClientTransfersigneddocuments 
			   (
			   DocumentId,
			   ClientId,
			   DocumentVersionId
			   )  
               SELECT 
			   Distinct D.DocumentId AS DocumentId
			   ,D.ClientId AS ClientId
			   ,DV.DocumentVersionId AS DocumentVersionId  
			   FROM Documents D  
               INNER JOIN DocumentCodes DC ON D.DocumentCodeId=DC.DocumentCodeId  
               INNER JOIN DocumentVersions DV ON D.CurrentDocumentVersionId=DV.DocumentVersionId  
               INNER JOIN DocumentSignatures DS ON D.DocumentId=DS.DocumentId  
               INNER JOIN Clients C ON D.ClientId=C.ClientId  
	           INNER JOIN #ClientTransferProgramdetails E ON E.ClientIdentifier=C.ClientId 
               WHERE ISNULL(D.RecordDeleted,'N')='N'  
               AND ISNULL(DC.RecordDeleted,'N')='N'  
               AND ISNULL(DV.RecordDeleted,'N')='N'  
               AND ISNULL(C.RecordDeleted,'N')='N'  
               AND (DATEDIFF(DAY, DS.SignatureDate,@StartDate1) <= 0)  
               AND (DATEDIFF(DAY, DS.SignatureDate,@EndDate1) >= 0)  
               AND DS.SignatureDate IS NOT NULL  
               AND DS.SignatureOrder=1 
 
          /************************************************************************************ 
                 Get the client transfered  Program details  based on input date range
          ************************************************************************************/  
             SELECT Distinct TSRD.DocumentVersionId
			 ,2 AS  RecordTypeCode  
			 ,CONCAT((ISNULL(PAS4G.GenderAtBirth,CASE WHEN C.Sex = 'F' THEN  '2' WHEN C.Sex ='M' THEN '1' ELSE NULL END)),ISNULL(FORMAT(PAS4G.DateOfBirth,'MMddyyyy'),Format(C.DOB,'MMddyyyy')),
              ISNULL((RIGHT(CONVERT(VARCHAR,PAS4G.SSN),4)),(RIGHT(CONVERT(VARCHAR,C.SSN),4))),ISNULL(UPPER(LEFT(PAS4G.LastName,2)),UPPER(LEFT(C.LastName,2)))) AS ProviderClientID
			 ,ISNULL((SELECT TOP 1 ExternalCode FROM ExternalMappings EM WHERE EM.RecordId=PAS4G.GenderAtBirth AND EM.Category='SEX' AND EM.TableName='GlobalCodes' AND EM.Purpose='NYPASReporting' AND ISNULL(EM.RecordDeleted,'N')='N'),
              (CASE RC.Sex WHEN 'M' THEN '1' WHEN 'F' THEN '2' ELSE '3' END))AS Sex
			 ,ISNULL(FORMAT(PAS4G.DateOfBirth,'MMddyyyy'),Format(C.DOB,'MMddyyyy')) AS BirthDate 
			 ,ISNULL(FORMAT(PAS4G.DateOfBirth,'MMddyyyy'),FORMAT(C.DOB,'ddMMyyyy')) AS ReportableBirthDate
			 ,ISNULL(RIGHT(PAS4G.SSN,4),ISNULL(RIGHT(C.SSN,4),'0000')) AS Last4SSN 
             ,ISNULL(CASE WHEN LEN([dbo].Csf_ReplaceSpecialCharacter(CDPAS44NY.LastNameAtBirth))>2 THEN UPPER(SUBSTRING([dbo].Csf_ReplaceSpecialCharacter(CDPAS44NY.LastNameAtBirth),1,2))  
              ELSE null END,
              CASE WHEN LEN([dbo].Csf_ReplaceSpecialCharacter(C.LastName))>2 THEN UPPER(SUBSTRING([dbo].Csf_ReplaceSpecialCharacter(C.LastName),1,2))  
              ELSE null END ) AS LastName2CHAR  
             ,CT.TransferfromProgramNo AS TransferfromProgramNo
             ,CT.TransferToProgramNo AS TransferToProgramNo
             ,CT.[Transferdatetime] AS Transferdate
			 ,CT.Transferdate AS [ReportableTransferdate]
             ,(Dbo.ssf_GetExternalMapping('PAS47','GlobalCodes','TreatmentEpisodeType',ISNULL(TE.TreatmentEpisodeType,-1),null,GETDATE())) AS ElementOfCareCode  
             ,(Dbo.ssf_GetExternalMapping('PAS47','GlobalCodes','TreatmentEpisodeType',ISNULL(TE.TreatmentEpisodeType,-1),null,GETDATE())) AS ReintegrationSettingCode  
             ,CASE WHEN ISNUMERIC(CDPAS44NY.LOCADTRASSMTSCORE)=1 THEN CDPAS44NY.LOCADTRASSMTSCORE  
              ELSE NULL END AS LocadTrasmtid
             ,CASE WHEN CDPAS44NY.LOCADTRASMTDATE IS NOT NULL THEN (CDPAS44NY.LOCADTRASMTDATE)  
              ELSE NULL END AS LocadTraSmtDate  
             ,CASE WHEN CDPAS44NY.LOCADTRASMTDATE IS NOT NULL THEN FORMAT(CDPAS44NY.LOCADTRASMTDATE, 'MMddyyyy')  
              ELSE NULL END AS [ReportableLocadTrasmtdate] 
             ,@ProviderNumber AS ProviderNo 
             ,CT.ClientIdentifier AS ClientIdentifier
			 ,'PAS 47-Client Transfer' AS [RecordIdentifier] 
			 ,NULL AS [RecordIdentifierId] 
             ,CT.SourceRecordIdentifier AS SourceRecordIdentifier
             ,CT.UniqueRecordIdentifier AS UniqueRecordIdentifier 
			 ,(  
				SELECT Top(1) DB.DacsBatchId FROM DacsBatches DB  
				INNER JOIN DacsBatchTypes DBT ON DB.DacsBatchTypeId=DBT.DacsBatchTypeId  
				INNER JOIN DacsBatchDetails DBD ON DBD.DacsBatchId=DB.DacsBatchId  
				INNER JOIN CustomNewYorkPASBatchClientTransfers CCT ON CCT.DacsBatchDetailId=DBD.DacsBatchDetailId  
				WHERE ISNULL(DB.RecordDeleted,'N')='N'  
				AND ISNULL(DBT.RecordDeleted,'N')='N'  
				AND ISNULL(DBD.RecordDeleted,'N')='N'  
				AND ISNULL(CCT.RecordDeleted,'N')='N'  
				AND DBT.DacsBatchTypeId=ISNULL(@PASBatchBatchTypeId,0)  
				AND CCT.RecordStatus=@AcceptedRecordStatusGCId  
				AND CCT.ClientIdentifier=CT.ClientIdentifier
				AND CCT.SourceRecordIdentifier=CT.SourceRecordIdentifier
				AND CCT.UniqueRecordIdentifier=CT.UniqueRecordIdentifier
				ORDER BY DacsBatchId DESC  
			  ) AS DacsBatchId 
             ,@DACS_InProgress_RecordStatusGCId AS[RecordStatus]
             ,NULL AS[RecordText] 
             ,NULL AS[ErrorMessage]
			 FROM #ClientTransfersigneddocuments TSRD
             INNER JOIN #ClientTransferProgramdetails CT ON CT.ClientIdentifier=TSRD.ClientId
             INNER JOIN Clients C ON C.ClientId=CT.ClientIdentifier 
             INNER JOIN CustomDocumentPAS44Generals CDPAS44G ON CDPAS44G.DocumentVersionId=TSRD.DocumentVersionId 
             INNER JOIN CustomDocumentPAS44NY CDPAS44NY ON CDPAS44NY.DocumentVersionId=CDPAS44G.DocumentVersionId
			 LEFT JOIN #MostRecentPAS44Document PAS4G ON PAS4G.ClientId=CT.ClientIdentifier
             LEFT JOIN TreatmentEpisodes TE ON CDPAS44G.TreatmentEpisodeId=TE.TreatmentEpisodeId
			 LEFT JOIN CustomClients CC ON CC.ClientId=C.ClientId AND ISNULL(CC.RecordDeleted,'N')='N'
             LEFT JOIN GlobalCodes GC2 ON GC2.GlobalCodeId=TE.TreatmentEpisodeType AND GC2.Category='TreatmentEpisodeType' 
             AND ISNULL(GC2.RecordDeleted,'N')='N' AND GC2.[Active]='Y' and CT.Transferdatetime BETWEEN @StartDate1 AND @EndDate1
			 LEFT JOIN TEDSEpisodes TDE ON TDE.ClientId=TSRD.ClientId AND ISNULL(TDE.RecordDeleted,'N')='N' AND TDE.TEDSEpisodeId=CDPAS44G.TEDSEpisodeId 
			 WHERE (@IsAllProgramReportingUnit = 1 
			 OR EXISTS(
						SELECT 1 FROM  GlobalCodes GCTDE 
                        WHERE TDE.EpisodeType=GCTDE.GlobalCodeId
                        AND GCTDE.Category = 'EpisodetoSubmit'
                        AND GCTDE.[Active]='Y'
                        AND ISNULL(GCTDE.RecordDeleted,'N')='N'
                        AND GCTDE.GlobalCodeId IN (SELECT ProgramReportingUnitId FROM @ProgramReportingUnits)						
					  )
			 )
			  
 END TRY  
   BEGIN CATCH  
             DECLARE @Error varchar(8000)                                                           
             SET @Error= Convert(varchar,ERROR_NUMBER()) + '*****' + Convert(varchar(4000),ERROR_MESSAGE())                                                                                         
             + '*****' + isnull(Convert(varchar,ERROR_PROCEDURE()),'csp_NewYorkPASGetClientTransferDetails')                                                                                         
             + '*****' + Convert(varchar,ERROR_LINE()) + '*****' + Convert(varchar,ERROR_SEVERITY())                                                                                          
             + '*****' + Convert(varchar,ERROR_STATE())                                       
            RAISERROR                                                                                         
              (                                                           
             @Error, -- Message text.                                                                                        
             16, -- Severity.                                                                                        
             1 -- State.                                                                                        
              );   
     END CATCH   
END


