object Settings {
  val testResultsFileName = "scalaTestLog.txt"
  val policyFileName = "allowAllPolicy"
  val submissionJsonFileName = "submission.json"
  val submissionJarFileName = "submittedSrc.jar"

  // time in seconds that we give scalatest for running
  val scalaTestTimeout = 240
  val individualTestTimeout = 30

  // default weight of each test in a GradingSuite, in case no weight is given
  val scalaTestDefaultWeigth = 10


  val scalaTestReportFileProperty = "scalatest.reportFile"
  val scalaTestIndividualTestTimeoutProperty = "scalatest.individualTestTimeout"
  val scalaTestReadableFilesProperty = "scalatest.readableFiles"
  val scalaTestDefaultWeigthProperty = "scalatest.defaultWeight"

  // debugging / developping options

  // don't decode json and unpack the submission sources, don't upload feedback
  val offlineMode = false
}
