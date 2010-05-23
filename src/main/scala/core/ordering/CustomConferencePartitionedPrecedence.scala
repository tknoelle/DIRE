package core.ordering

import scala.collection.mutable.{Map => MMap}

/**
 * User: nowi
 * Date: 19.01.2010
 * Time: 20:58:41
 */

class CustomConferencePartitionedPrecedence extends Precedence {
  // create the internaly used string comperator

  val cache: MMap[(String, String), Int] = scala.collection.mutable.HashMap[(String, String), Int]()

  // build the precedence list
  val precedence = List("skf40", "skf41", "skf42", "skf43", "skf44", "skf45", "skf46", "skf47",
    "skf48", "skf49", "skf410", "skf411", "skf412", "skf413", "skf414", "skf415", "skf416",
    "skf417", "skf418", "skf419", "skf420", "skf421", "skf422", "skf423", "skf424", "skf425",
    "skf426", "skf427", "skf428", "skf429", "skf30", "skf31", "skf32", "skf33", "skf34",
    "skf35", "skf36", "skf37", "skf38", "skf39", "skf310", "skf311", "skf312", "skf313", "skf314",
    "skf315", "skf316", "skf317", "skf318", "skf319", "skf320", "skf321", "skf322", "skf323", "skf324", "skf325", "skf326", "skf327", "skf328", "skf329", "skf330", "skf331", "skf332", "skf333", "skf334", "skf335", "skf336", "skf337", "skf338", "skf339", "skf340", "skf341", "skf342", "skf343", "skf344", "skf345", "skf346", "skf347", "skf348", "skf349", "skf350", "skf351", "skf352", "skf353", "skf354", "skf355", "skf356", "skf357", "skf358", "skf359", "skf360", "skf361", "skf362", "skf363", "skf364", "skf365", "skf366", "skf367", "skf368", "skf369", "skf370", "skf371", "skf372", "skf373", "skf374", "skf375", "skf376", "skf377", "skf378", "skf379", "skf380", "skf381", "skf382", "skf383", "skf384", "skf385", "skf386", "skf387", "skf388", "skf389", "skf390", "skf391", "skf392", "skf393", "skf394", "skf395", "skf396", "skf397", "skf398", "skf399", "skf3100", "skf3101", "skf3102", "skf3103", "skf3104", "skf3105", "skf3106", "skf3107", "skf3108", "skf3109", "skf3110", "skf3111", "skf3112", "skf3113", "skf3114", "skf3115", "skf3116", "skf3117", "skf3118", "skf3119", "skf3120", "skf3121", "skf3122", "skf3123", "skf3124", "skf3125", "skf3126", "skf3127", "skf3128", "skf3129", "skf3130", "skf3131", "skf3132", "skf3133", "skf3134", "skf3135", "skf3136", "skf3137", "skf3138", "skf3139", "skf3140", "skf3141", "skf3142", "skf3143", "skf3144", "skf3145", "skf3146", "skf3147", "skf3148", "skf3149", "skf3150", "skf3151", "skf3152", "skf3153", "skf3154", "skf3155", "skf3156", "skf3157", "skf3158", "skf3159", "skf3160", "skf3161", "skf3162", "skf3163", "skf3164", "skf3165", "skf3166", "skf3167", "skf3168", "skf3169", "skf3170", "skf3171", "skf3172", "skf3173", "skf3174", "skf3175", "skf3176", "skf3177", "skf3178", "skf3179", "skf3180", "skf3181", "skf3182", "skf3183", "skf3184", "skf3185", "skf3186", "skf3187", "skf3188", "skf3189", "skf3190", "skf3191", "skf3192", "skf3193", "skf3194", "skf3195", "skf3196", "skf3197", "skf3198", "skf3199", "skf3200", "skf3201", "skf3202", "skf3203", "skf3204", "skf3205", "skf3206", "skf3207", "skf3208", "skf3209", "skf3210", "skf3211", "skf3212", "skf3213", "skf3214", "skf3215", "skf3216", "skf3217", "skf3218", "skf3219", "skf3220", "skf3221", "skf3222", "skf3223", "skf3224", "skf3225", "skf3226", "skf3227", "skf3228", "skf3229", "skf3230", "skf3231", "skf3232", "skf3233", "skf3234", "skf3235", "skf3236", "skf3237", "skf3238", "skf3239", "skf3240", "skf3241", "skf3242", "skf3243", "skf3244", "skf3245", "skf3246", "skf3247", "skf3248", "skf3249", "skf3250", "skf3251", "skf3252", "skf3253", "skf3254", "skf3255", "skf3256", "skf3257", "skf3258", "skf3259", "skf3260", "skf3261", "skf3262", "skf3263", "skf3264", "skf3265", "skf00", "skf01", "skf02", "skf03", "skf04", "skf05", "skf06", "skf07", "skf08", "skf09", "skf10", "skf11", "skf12", "skf13", "skf14", "skf15", "skf16", "skf17", "skf18", "skf19", "skf110", "skf111", "skf112", "skf113", "skf114", "skf115", "skf116", "skf117", "skf118", "skf119", "skf120", "skf121", "skf122", "skf123", "skf124", "skf125", "skf126", "skf127", "skf128", "skf129", "skf130", "skf131", "skf132", "skf133", "skf134", "skf135", "skf136", "skf137", "skf138", "skf139", "skf140", "skf141", "skf142", "skf143", "skf144", "skf145", "skf146", "skf147", "skf20", "skf21", "skf22", "skf23", "skf24", "skf25", "skf26", "skf27", "skf28", "skf29", "skf210", "skf211", "skf212", "skf213", "skf214", "skf215", "skf216", "skf217", "skf218", "skf219", "skf220", "skf221", "div", "id", "O4Document", "O4NEWATOMIC31", "O4NEWATOMIC30", "O4Startofconference", "O4OrganizingCommittee", "O4NEWATOMIC20", "O4Email", "O4Webmaster", "O4searchedby", "O4NEWATOMIC14", "O4awardedby", "O4NEWATOMIC29", "O4Nation", "O4Exhibitor", "O4Authorofpaper", "O4ACMSIGKDD", "O4BronzeSupporter", "O4RegistrationSIGKDDMember", "O4Listener", "O4obtain", "O4submituntil", "O4NEWATOMIC13", "O4Speaker", "O4NEWATOMIC32", "O4design", "O4Paper", "O4Nameofconference", "O4OrganizingCommitteemember", "O4NEWATOMIC12", "O4NEWATOMIC27", "O4BestStudentPaperSupporter", "O4NEWATOMIC3", "O4NEWATOMIC6", "O4Hotel", "O4NEWATOMIC11", "O4BestApplicationsPaperAward", "O4GoldSupporter", "O4NEWATOMIC25", "O4presentation", "O4Person", "O4canstayin", "O4Sponzorfee", "O4Nameofsponsor", "O4NEWATOMIC33", "O4NEWATOMIC28", "O4NEWATOMIC17", "O4PlatinumSupporter", "O4NEWATOMIC19", "O4Sponzor", "O4NEWATOMIC2", "O4Cityofconference", "O4NEWATOMIC9", "O4NEWATOMIC24", "O4NEWATOMIC22", "O4BestResearchPaperAward", "O4Organizator", "O4presentationedby", "O4search", "O4GeneralChair", "O4notificationuntil", "O4DeadlinePaperSubmission", "O4Conference", "O4NEWATOMIC8", "O4submit", "O4Author", "O4InvitedSpeaker", "O4award", "O4BestPaperAwardsCommittee", "O4DeadlineAbstractSubmission", "O4Deadline", "O4RegistrationSIGMODMember", "O4ProgramChair", "O4Review", "O4DeadlineAuthornotification", "O4Registrationfee", "O4payedby", "O4pay", "O4designedby", "O4NEWATOMIC5", "O4Endofconference", "O4NEWATOMIC23", "O4hold", "O4Authorofpaperstudent", "O4ProgramCommittee", "O4Currency", "O4RegistrationStudent", "O4SilverSupporter", "O4ProgramCommitteemember", "O4Date", "O4NEWATOMIC1", "O4NEWATOMIC15", "O4NEWATOMIC4", "O4NEWATOMIC0", "O4Abstract", "O4Mainoffice", "O4Award", "O4holdedby", "O4NEWATOMIC16", "O4Name", "O4Price", "O4Place", "O4Committee", "O4Conferencehall", "O4NEWATOMIC26", "O4Fee", "O4NEWATOMIC7", "O4NEWATOMIC21", "O4NEWATOMIC18", "O4NEWATOMIC10", "O4RegistrationNonMember", "O4BestStudentPaperAward", "O3NEWATOMIC90", "O3ispaidin", "O3Creditcard", "O3Audiovisualequipment", "O3Introduction", "O3Activityafterconference", "O3Person", "O3NEWATOMIC38", "O3write", "O3hasamountof", "O3NEWATOMIC67", "O3Delegate", "O3Conferenceairport", "O3Conferencecity", "O3NEWATOMIC10", "O3NEWATOMIC26", "O3NEWATOMIC37", "O3Initialmanuscipt", "O3Authorcdproceedingsincluded", "O3NEWATOMIC24", "O3Registrationform", "O3Socialprogram", "O3NEWATOMIC13", "O3Reviewer", "O3Sponzorship", "O3Conferencebuilding", "O3NEWATOMIC93", "O3NEWATOMIC4", "O3Videocassetteplayer", "O3Sponsorstate", "O3Presenterhouse", "O3Renting", "O3NEWATOMIC27", "O3Activitybeforeconference", "O3Transportvehicle", "O3Receivingmanuscript", "O3NEWATOMIC5", "O3Banktransfer", "O3Modelling", "O3Author", "O3NEWATOMIC92", "O3Hotelregistrationform", "O3Authorbookproceedingsincluded", "O3NEWATOMIC51", "O3isdatedon", "O3speakin", "O3NEWATOMIC85", "O3NEWATOMIC58", "O3NEWATOMIC60", "O3NEWATOMIC14", "O3NEWATOMIC94", "O3NEWATOMIC39", "O3Speakerlecture", "O3Acceptingmanuscript", "O3Currency", "O3Workerlecturer", "O3need", "O3Registrationfee", "O3Taxi", "O3Authorinformationform", "O3NEWATOMIC23", "O3Singlehotelroom", "O3Conferenceactivity", "O3Memeberregistrationfee", "O3Conferencehall", "O3Plenarylecture", "O3Shuttlebus", "O3NEWATOMIC95", "O3NEWATOMIC69", "O3NEWATOMIC6", "O3NEWATOMIC75", "O3State", "O3Paymentdocument", "O3Tutorial", "O3give", "O3NEWATOMIC11", "O3Technicalcommitee", "O3Publication", "O3isvisitedby", "O3Sponsor", "O3obtain", "O3NEWATOMIC29", "O3NEWATOMIC20", "O3Activity", "O3ismadefrom", "O3Camerareadymanuscriptdeadline", "O3pay", "O3Hotelroom", "O3NEWATOMIC57", "O3NEWATOMIC59", "O3NEWATOMIC78", "O3Coffeebreak", "O3Feeforextratrip", "O3Form", "O3NEWATOMIC83", "O3issentafter", "O3NEWATOMIC2", "O3Sessionchair", "O3Fulldaytour", "O3NEWATOMIC25", "O3NEWATOMIC22", "O3isusedfor", "O3Authorattendeebookregistrationfee", "O3NEWATOMIC30", "O3Overheadprojector", "O3ispaidwith", "O3Car", "O3Videopresentation", "O3NEWATOMIC56", "O3send", "O3Technicactivity", "O3NEWATOMIC50", "O3Listener", "O3NEWATOMIC62", "O3NEWATOMIC61", "O3Nonauthorregistrationfee", "O3Conferencestate", "O3NEWATOMIC49", "O3isheldin", "O3Presentation", "O3Money", "O3Deadline", "O3Cheque", "O3NEWATOMIC9", "O3Studentnonspeaker", "O3Introductionofspeaker", "O3NEWATOMIC18", "O3NEWATOMIC48", "O3Van", "O3Lecturer", "O3NEWATOMIC16", "O3NEWATOMIC35", "O3NEWATOMIC54", "O3NEWATOMIC80", "O3ispresent", "O3Deadlinehotelreservation", "O3isusedby", "O3Hotelfee", "O3Document", "O3Fee", "O3NEWATOMIC28", "O3Recordofattendance", "O3NEWATOMIC88", "O3Tripday", "O3NEWATOMIC73", "O3Departuretax", "O3NEWATOMIC15", "O3occupy", "O3isheldbefore", "O3Conferencerestaurant", "O3Studentregistrationfee", "O3isneededfor", "O3Computer", "O3Coctailreception", "O3Tripcity", "O3NEWATOMIC89", "O3NEWATOMIC40", "O3NEWATOMIC45", "O3Deadlinefornotificationofacceptance", "O3Time", "O3NEWATOMIC44", "O3ispaidby", "O3Bookproceeding", "O3NEWATOMIC52", "O3Viza", "O3ispreparedby", "O3NEWATOMIC72", "O3sign", "O3Speaker", "O3Sponsorcity", "O3Doublehotelroom", "O3NEWATOMIC66", "O3isconnectedwith", "O3Tutorialspeaker", "O3isoccupiedby", "O3NEWATOMIC17", "O3Review", "O3isgivento", "O3issignedby", "O3ispaidfor", "O3isdesignedfor", "O3NEWATOMIC53", "O3Mailinglist", "O3Conferencehotel", "O3Presenterstate", "O3Refusingmanuscript", "O3NEWATOMIC43", "O3NEWATOMIC46", "O3Item", "O3NEWATOMIC3", "O3NEWATOMIC81", "O3Registationdeadline", "O3NEWATOMIC47", "O3NEWATOMIC55", "O3NEWATOMIC21", "O3IASTEDmember", "O3isgivenby", "O3Sessionroom", "O3Studentlecturer", "O3donetill", "O3Presenteruniversity", "O3NEWATOMIC91", "O3Welcomeaddress", "O3Research", "O3Mainoffice", "O3NEWATOMIC71", "O3Onedaypresenter", "O3Card", "O3NEWATOMIC0", "O3prepare", "O3Workernonspeaker", "O3isheldafter", "O3Invitationletter", "O3NEWATOMIC42", "O3issentby", "O3NEWATOMIC31", "O3Valueaddedtax", "O3Session", "O3NEWATOMIC8", "O3NEWATOMIC19", "O3NEWATOMIC77", "O3NEWATOMIC68", "O3NEWATOMIC33", "O3Submission", "O3Building", "O3Nonspeaker", "O3Finalmanuscript", "O3ConferenceHiker", "O3NEWATOMIC63", "O3NEWATOMIC41", "O3gothrough", "O3issentbefore", "O3NEWATOMIC76", "O3NEWATOMIC86", "O3NEWATOMIC65", "O3Dinnerbanquet", "O3Simulating", "O3Transparency", "O3Authorattendeecdregistrationfee", "O3iswritenby", "O3BriefintroductionforSessionchair", "O3PowerPointpresentation", "O3Tax", "O3ispresentin", "O3Presentercity", "O3Hotelpresenter", "O3IASTEDnonmember", "O3NEWATOMIC79", "O3NEWATOMIC36", "O3NEWATOMIC32", "O3isequippedby", "O3Oneconferenceday", "O3NEWATOMIC64", "O3LCDprojector", "O3City", "O3Departure", "O3Registration", "O3Tip", "O3NEWATOMIC70", "O3NEWATOMIC74", "O3NEWATOMIC1", "O3NEWATOMIC12", "O3Nonmemberregistrationfee", "O3Place", "O3NEWATOMIC34", "O3Sponsorcompanyhouse", "O3issituatedin", "O3Submissionsdeadline", "O3NEWATOMIC7", "O3NEWATOMIC87", "O3Conferencedays", "O3NEWATOMIC82", "O3NEWATOMIC84", "O3Cdproceening", "O3Lecture", "O3Plenarylecturespeaker", "O0NEWATOMIC29", "O0NEWATOMIC59", "O0date", "O0Document", "O0hasConflictOfInterest", "O0NEWATOMIC38", "O0PaperAbstract", "O0NEWATOMIC37", "O0NEWATOMIC22", "O0Person", "O0rejectedBy", "O0NEWATOMIC34", "O0hasBeenAssigned", "O0NEWATOMIC46", "O0reviewsPerPaper", "O0NEWATOMIC15", "O0adjustedBy", "O0memberOfProgramCommittee", "O0hasConferenceMember", "O0assignReviewer", "O0title", "O0hasBid", "O0assignedByAdministrator", "O0acceptsHardcopySubmissions", "O0Acceptance", "O0NEWATOMIC36", "O0NEWATOMIC27", "O0NEWATOMIC23", "O0hasCoauthor", "O0NEWATOMIC14", "O0NEWATOMIC13", "O0Paper", "O0NEWATOMIC9", "O0writeReview", "O0detailsEnteredBy", "O0NEWATOMIC40", "O0Decision", "O0NEWATOMIC30", "O0NEWATOMIC25", "O0NEWATOMIC41", "O0assignExternalReviewer", "O0NEWATOMIC24", "O0finalizePaperAssignment", "O0NEWATOMIC19", "O0NEWATOMIC44", "O0Conference", "O0Bid", "O0virtualMeetingEnabledBy", "O0paperAssignmentToolsRunBy", "O0NEWATOMIC50", "O0SubjectArea", "O0acceptPaper", "O0Reviewer", "O0NEWATOMIC51", "O0NEWATOMIC28", "O0Author", "O0NEWATOMIC17", "O0NEWATOMIC16", "O0hasSubjectArea", "O0setMaxPapers", "O0NEWATOMIC33", "O0NEWATOMIC5", "O0maxPapers", "O0enterConferenceDetails", "O0acceptedBy", "O0NEWATOMIC39", "O0Review", "O0NEWATOMIC56", "O0paperAssignmentFinalizedBy", "O0NEWATOMIC55", "O0AuthorNotReviewer", "O0writePaper", "O0logoURL", "O0NEWATOMIC4", "O0NEWATOMIC35", "O0NEWATOMIC53", "O0NEWATOMIC18", "O0ProgramCommitteeMember", "O0reviewCriteriaEnteredBy", "O0adjustBid", "O0Administrator", "O0startReviewerBidding", "O0readByReviewer", "O0NEWATOMIC26", "O0addedBy", "O0assignedTo", "O0NEWATOMIC52", "O0runPaperAssignmentTools", "O0ExternalReviewer", "O0NEWATOMIC49", "O0readByMetaReviewer", "O0NEWATOMIC8", "O0NEWATOMIC2", "O0AssociatedChair", "O0siteURL", "O0paperID", "O0NEWATOMIC32", "O0name", "O0cowritePaper", "O0enableVirtualMeeting", "O0rejectPaper", "O0email", "O0printHardcopyMailingManifests", "O0NEWATOMIC54", "O0submitPaper", "O0enterReviewCriteria", "O0memberOfConference", "O0NEWATOMIC58", "O0NEWATOMIC31", "O0NEWATOMIC6", "O0hasProgramCommitteeMember", "O0ProgramCommittee", "O0NEWATOMIC48", "O0NEWATOMIC7", "O0hardcopyMailingManifestsPrintedBy", "O0Chairman", "O0NEWATOMIC1", "O0markConflictOfInterest", "O0PaperFullVersion", "O0MetaReviewer", "O0ConferenceChair", "O0reviewerBiddingStartedBy", "O0Rejection", "O0ConferenceMember", "O0hasDecision", "O0NEWATOMIC10", "O0NEWATOMIC42", "O0NEWATOMIC57", "O0NEWATOMIC47", "O0NEWATOMIC11", "O0assignedByReviewer", "O0writtenBy", "O0Preference", "O0NEWATOMIC20", "O0addProgramCommitteeMember", "O0ProgramCommitteeChair", "O0NEWATOMIC12", "O0Coauthor", "O0NEWATOMIC45", "O0User", "O0NEWATOMIC21", "O0MetaReview", "O0NEWATOMIC43", "O0NEWATOMIC3", "O0readPaper", "O0hasAuthor", "O0NEWATOMIC0", "O0endReview", "O1Registrationofparticipantsevent", "O1University", "O1hasAdministrativeEvent", "O1NEWATOMIC31", "O1hasTopic", "O1NEWATOMIC5", "O1Conference", "O1Administrativeevent", "O1Reception", "O1Student", "O1hasStreet", "O1Member", "O1defaultChoice", "O1location", "O1NEWATOMIC8", "O1expertOn", "O1Administrator", "O1NEWATOMIC33", "O1Volunteer", "O1abstract", "O1hasshorttitle", "O1endson", "O1Assistant", "O1hasEmail", "O1NEWATOMIC17", "O1Event", "O1Country", "O1Workshop", "O1contactEmail", "O1NEWATOMIC2", "O1NEWATOMIC23", "O1NEWATOMIC25", "O1NEWATOMIC28", "O1NEWATOMIC29", "O1Trip", "O1hasPhone", "O1NEWATOMIC22", "O1Organization", "O1hasPostalCode", "O1NEWATOMIC13", "O1Reviewingevent", "O1studyAt", "O1NEWATOMIC32", "O1NEWATOMIC12", "O1NEWATOMIC21", "O1CameraReadyevent", "O1hasCity", "O1hasFirstName", "O1hasVAT", "O1employedBy", "O1parallelwith", "O1Socialevent", "O1writtenBy", "O1NEWATOMIC14", "O1Contribution", "O1NEWATOMIC20", "O1NEWATOMIC9", "O1Shortpaper", "O1Author", "O1hasCountry", "O1remark", "O1NEWATOMIC16", "O1ScienceWorker", "O1maxChoice", "O1hasHomePage", "O1Regular", "O1hasTitle", "O1NEWATOMIC0", "O1Company", "O1ChairPC", "O1hasKeyword", "O1writes", "O1Workingevent", "O1hastitle", "O1NEWATOMIC11", "O1NEWATOMIC19", "O1Topic", "O1reviewes", "O1Paper", "O1Banquet", "O1City", "O1NEWATOMIC18", "O1NEWATOMIC4", "O1Participant", "O1NEWATOMIC10", "O1NEWATOMIC35", "O1hasSurname", "O1NEWATOMIC6", "O1NEWATOMIC3", "O1Submissionevent", "O1NEWATOMIC30", "O1Person", "O1minChoice", "O1earlyRegistration", "O1NEWATOMIC27", "O1Scholar", "O1Poster", "O1NEWATOMIC15", "O1Reviewingresultsevent", "O1MemberPC", "O1follows", "O1Tutorial", "O1startson", "O1NEWATOMIC34", "O1NEWATOMIC1", "O1NEWATOMIC7", "O1hasFax", "O1NEWATOMIC24", "O1dealsWith", "O1NEWATOMIC26", "O2locationOf", "O2AcceptedPaper", "O2Track", "O2PossibleReviewer", "O2RejectedPaper", "O2organisedBy", "O2Event", "O2Flyer", "O2partOfEvent", "O2WorkshopSession", "O2WorkshopPaper", "O2ResearchTopic", "O2TutorialAbstract", "O2IndividualPresentation", "O2topicCoveredBy", "O2SessionChair", "O2heldIn", "O2OCMember", "O2AgencyStaffMember", "O2NEWATOMIC26", "O2ConferenceProceedings", "O2NEWATOMIC32", "O2AssignedPaper", "O2OrganisingAgency", "O2NEWATOMIC29", "O2SocialEvent", "O2ConferenceTrip", "O2PaperAuthor", "O2NEWATOMIC39", "O2ResearchInstitute", "O2NEWATOMIC30", "O2NEWATOMIC9", "O2hasEvent", "O2NEWATOMIC22", "O2NEWATOMIC12", "O2Paper", "O2NEWATOMIC10", "O2ProgrammeBrochure", "O2hasReviewer", "O2NEWATOMIC23", "O2updatedVersionOf", "O2DemoPaper", "O2Location", "O2Presenter", "O2Person", "O2NEWATOMIC21", "O2PosterPaper", "O2eventOnList", "O2IndustrialSession", "O2PCChair", "O2EvaluatedPaper", "O2presentationOfPaper", "O2EarlyRegisteredParticipant", "O2Session", "O2NEWATOMIC14", "O2scientificallyOrganises", "O2IndustrialPaper", "O2NEWATOMIC40", "O2scientificallyOrganisedBy", "O2Document", "O2NEWATOMIC4", "O2ConferenceBanquet", "O2NEWATOMIC18", "O2CameraReadyPaper", "O2coversTopic", "O2PositiveReview", "O2TutorialChair", "O2NEWATOMIC15", "O2NEWATOMIC17", "O2NEWATOMIC1", "O2NEWATOMIC8", "O2Workshop", "O2MultiauthorVolume", "O2ScientificEvent", "O2Tutorial", "O2InvitedSpeaker", "O2NEWATOMIC3", "O2Organisation", "O2NEWATOMIC36", "O2ContributedTalk", "O2NEWATOMIC35", "O2InvitedTalkAbstract", "O2University", "O2NEWATOMIC28", "O2NEWATOMIC20", "O2NEWATOMIC33", "O2NEWATOMIC24", "O2NEWATOMIC31", "O2publisherOf", "O2reviewWrittenBy", "O2hasReview", "O2authorOf", "O2WebSite", "O2SCMember", "O2RegularSession", "O2Review", "O2ProceedingsPublisher", "O2DemoChair", "O2PosterSession", "O2SubmittedPaper", "O2NEWATOMIC11", "O2NEWATOMIC13", "O2ConferenceParticipant", "O2NEWATOMIC5", "O2NEWATOMIC19", "O2technicallyOrganises", "O2LateRegisteredParticipant", "O2NEWATOMIC41", "O2writtenBy", "O2AcademicInstitution", "O2NEWATOMIC6", "O2NEWATOMIC7", "O2paperPresentedAs", "O2PCMember", "O2Abstract", "O2NEWATOMIC38", "O2WorkshopChair", "O2NEWATOMIC16", "O2reviewerOfPaper", "O2ConferenceSession", "O2InvitedTalk", "O2NegativeReview", "O2NEWATOMIC37", "O2Proceedings", "O2organises", "O2volumeContainsPaper", "O2reviewOfPaper", "O2NeutralReview", "O2NEWATOMIC25", "O2OCChair", "O2RegularPaper", "O2NEWATOMIC34", "O2inverseofpartOf7", "O2NEWATOMIC27", "O2DemoSession", "O2Conference", "O2Student", "O2NEWATOMIC2", "O2listsEvent", "O2ConferencePaper", "O2hasUpdatedVersion", "O2NEWATOMIC0")
  // extract the symbolic names from the clauses , and compare with default precedence



  private def doCompare(a: String, b: String) = {
    precedence.indexOf(b) compare precedence.indexOf(a)
  }

  override def compare(a: String, b: String) = {
    // check if we already compared this, if not add to cache
    cache.getOrElseUpdate((a, b), doCompare(a, b))

  }


}