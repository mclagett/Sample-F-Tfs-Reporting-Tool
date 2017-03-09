using System;
using System.Collections.Generic;
using System.Data.SqlTypes;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using Word = Microsoft.Office.Interop.Word;
using System.Runtime.Serialization;
using System.IO;
using System.Runtime.Serialization.Json;
using Microsoft.TeamFoundation.WorkItemTracking.Client;
using ProjectTimelineGeneratorLib.Domain;

namespace ProjectTimelineGenerator
{
    class Program
    {
        private static Word.Application WordApp { get; set; }
        private static Word.Document CurrentIterationTimeLineDoc { get; set; }
        private static Word.Document PreviousIterationTimeLineDoc { get; set; }
        private static Word.Document TimeLineSummaryDoc { get; set; }

        private static Word.Document createDocument()
        {
            return WordApp.Documents.Add();
        }

       private static Word.Document openDocument(string filePath)
        {
            Console.WriteLine(String.Format("Opening {0}...", filePath));
            return WordApp.Documents.Open(filePath);
        }

        // outputs Feature Name heading to target Word doc
        private static Word.Paragraph outputFeatureName(Word.Document doc, string featureName,bool suppressPageBreak = false,
                   Word.WdColor fontColor = Word.WdColor.wdColorAutomatic)
        {
            Word.Paragraph newParagraph;
            if (!(doc.Paragraphs.Count == 1) && !(suppressPageBreak))
            {
                newParagraph = doc.Paragraphs.Add();
                newParagraph.Range.InsertBreak(Word.WdBreakType.wdPageBreak);
            }
            newParagraph = doc.Paragraphs.Add();
            newParagraph.Range.Text = "Feature: " + featureName;
            newParagraph.set_Style(doc.Styles["Heading 3"]);
            newParagraph.Range.Font.Size = 24;
            newParagraph.Range.Font.Bold = -1;
            newParagraph.Range.Font.Color = fontColor;
            newParagraph.KeepWithNext = 0;
            newParagraph.Range.InsertParagraphAfter();
            return newParagraph;
        }


        // outputs Iteration Name heading to target Word doc
        private static Word.Paragraph outputIterationName(Word.Document doc, string iterationText, bool suppressPageBreak,
                   Word.WdColor fontColor = Word.WdColor.wdColorAutomatic, bool overrideDefaultPrint = false)
        {
            Word.Paragraph newParagraph;
            var lastParagraphPageNum = doc.Paragraphs.Last.Previous(1).Range.Information[Word.WdInformation.wdActiveEndPageNumber];
            doc.Content.Collapse(Word.WdCollapseDirection.wdCollapseEnd);
            var currentPageNum = doc.Content.Information[Word.WdInformation.wdActiveEndPageNumber];
            if (!doc.Paragraphs.Last.Previous(1).Range.Text.Contains("Feature:") &&
                (lastParagraphPageNum == currentPageNum) && (!suppressPageBreak))
            {
                newParagraph = doc.Paragraphs.Add();
                newParagraph.Range.InsertBreak(Word.WdBreakType.wdPageBreak);
            }
            newParagraph = doc.Paragraphs.Add();
            if (overrideDefaultPrint == true)
                newParagraph.Range.Text = iterationText;
            else
                newParagraph.Range.Text = "Iteration: " + iterationText;
            newParagraph.set_Style(doc.Styles["Heading 2"]);
            newParagraph.Range.Font.Size = 20;
            newParagraph.Range.Font.Bold = -1;
            newParagraph.Range.Font.Color = fontColor;
            newParagraph.KeepWithNext = 0;
            newParagraph.Range.InsertParagraphAfter();
            return newParagraph;
        }

        // outputs Sub Heading text to target Word doc
        private static Word.Paragraph outputSubHeading(Word.Document doc, string subHeadingText, 
                   Word.WdColor fontColor = Word.WdColor.wdColorAutomatic)
        {
            Word.Paragraph newParagraph;
            var lastParagraphPageNum = doc.Paragraphs.Last.Previous(1).Range.Information[Word.WdInformation.wdActiveEndPageNumber];
            doc.Content.Collapse(Word.WdCollapseDirection.wdCollapseEnd);
            var currentPageNum = doc.Content.Information[Word.WdInformation.wdActiveEndPageNumber];
            if (!doc.Paragraphs.Last.Previous(1).Range.Text.Contains("Iteration") &&
                lastParagraphPageNum == currentPageNum)
            {
                newParagraph = doc.Paragraphs.Add();
                newParagraph.Range.InsertBreak(Word.WdBreakType.wdPageBreak);
            }
            newParagraph = doc.Paragraphs.Add();
            newParagraph.Range.Text = subHeadingText;
            newParagraph.set_Style(doc.Styles["Heading 2"]);
            newParagraph.Range.Font.Size = 16;
            newParagraph.Range.Font.Bold = -1;
            newParagraph.Range.Font.Color = fontColor;
            newParagraph.Range.Font.Underline = Word.WdUnderline.wdUnderlineSingle;
            newParagraph.Range.ParagraphFormat.SpaceBefore = 24;
            newParagraph.KeepWithNext = 0;
            newParagraph.Range.InsertParagraphAfter();
            return newParagraph;
        }

        // outputs User Story name to target word doc
        private static Word.Paragraph insertUserStoryName(Word.Document doc, userStory s,
                   Word.WdColor fontColor = Word.WdColor.wdColorAutomatic)
        {
            var newParagraph = doc.Paragraphs.Add();
            newParagraph.Range.Text = s.userStory.Title + ":";
            newParagraph.set_Style(doc.Styles["Heading 1"]);
            newParagraph.Range.Font.Size = 14;
            newParagraph.Range.Font.Bold = -1;
            newParagraph.Range.Font.Color = fontColor;
            newParagraph.Range.ParagraphFormat.SpaceBefore = 24;
            newParagraph.KeepWithNext = -1;
            newParagraph.Range.InsertParagraphAfter();
            return newParagraph;
        }

        // outputs plain paragraph text to target word doc
        private static Word.Paragraph outputParagraphText(Word.Document doc, string textToOutput)
        {
            var newParagraph = doc.Paragraphs.Add();
            newParagraph.Range.Text = textToOutput;
            newParagraph.Range.InsertParagraphAfter();
            newParagraph.set_Style(doc.Styles["Normal"]);
            newParagraph.Range.Font.Size = 12;
            newParagraph.KeepWithNext = -1;
            return newParagraph;
        }

        // wraps incoming string into an html document and saves it to 
        // temporary file folder so that it can be inserted into a word doc
        private static string SaveToTemporaryFile(string html)
        {
            string htmlTempFilePath = Path.Combine(Path.GetTempPath(), string.Format("{0}.html", Path.GetRandomFileName()));
            using (StreamWriter writer = File.CreateText(htmlTempFilePath))
            {
                html = string.Format("<html>{0}</html>", html);

                writer.WriteLine(html);
            }

            return htmlTempFilePath;
        }

        // inserts incoming html formatted string into target word doc 
        private static Word.Paragraph outputParagraphTextHtml(Word.Document doc, string textToOutput)
        {
            object missing = Type.Missing;
            var newParagraph = doc.Paragraphs.Add();
            newParagraph.Range.InsertFile(SaveToTemporaryFile(textToOutput), 
                ref missing, ref missing, ref missing, ref missing);
            newParagraph.Range.InsertParagraphAfter();
            newParagraph.set_Style(doc.Styles["Normal"]);
            newParagraph.Range.Font.Size = 12;
            newParagraph.KeepWithNext = -1;
            return newParagraph;
        }

        // writes incoming contents to a Word table cell
        private static void fillCol(int i, int j, Word.Table table, object content)
        {
            table.Cell(i, j).Range.Text = content.ToString();
        }

        // adds a table of specified column size to taget word document
        private static Word.Table addTableToDocument(Word.Document doc, int columnCount)
        {
            var oMissing = System.Reflection.Missing.Value;
            var oEndOfDoc = "\\endofdoc";
            var endOfDocRange = doc.Bookmarks.get_Item(oEndOfDoc).Range;
            var table = doc.Tables.Add(endOfDocRange, 1, columnCount, oMissing, oMissing);
            table.Range.ParagraphFormat.SpaceAfter = 6.0f;
            table.Range.Font.Size = 10;
            table.Range.ParagraphFormat.KeepWithNext = 0;
            table.Range.Borders[Word.WdBorderType.wdBorderLeft].LineStyle = Word.WdLineStyle.wdLineStyleSingle;
            table.Range.Borders[Word.WdBorderType.wdBorderRight].LineStyle = Word.WdLineStyle.wdLineStyleSingle;
            table.Range.Borders[Word.WdBorderType.wdBorderTop].LineStyle = Word.WdLineStyle.wdLineStyleSingle;
            table.Range.Borders[Word.WdBorderType.wdBorderBottom].LineStyle = Word.WdLineStyle.wdLineStyleSingle;

            return table;
        }

        // add a row representing a closed task commitment to a closed task table
        private static void addClosedTaskRow(developerTaskCommitment dtc, Word.Table table)
        {
            Word.Row row = table.Rows.Add();
            int rowIndex = row.Index;
            fillCol(rowIndex, 1, table, dtc.taskId);
            fillCol(rowIndex, 2, table, "Task");
            fillCol(rowIndex, 3, table, dtc.taskTitle);
            fillCol(rowIndex, 4, table, dtc.taskState);
            fillCol(rowIndex, 5, table, dtc.originalEstimate);
            fillCol(rowIndex, 6, table, dtc.completedWork);
            fillCol(rowIndex, 7, table, dtc.activatedDate);
            fillCol(rowIndex, 8, table, dtc.completedDate);
            fillCol(rowIndex, 9, table, dtc.committedDeveloper.developerName);
            Console.WriteLine("Row {0} has printed successfully", rowIndex);
        }

        // add header to a closed task table 
        private static void addClosedTaskHeader(Word.Table table)
        {
            fillCol(1, 1, table, "Id");
            fillCol(1, 2, table, "Work Item Type");
            fillCol(1, 3, table, "Title");
            fillCol(1, 4, table, "State");
            fillCol(1, 5, table, "Original Estimate");
            fillCol(1, 6, table, "Completed Hours");
            fillCol(1, 7, table, "Date Activated");
            fillCol(1, 8, table, "Date Completed");
            fillCol(1, 9, table, "Assigned To");
            table.Rows[1].HeadingFormat = -1;
            Console.WriteLine("Header row has printed successfully");
        }

        // create a closed task table in target word doc to display closed tasks
        // these will added for individual iteration/user stories
        private static void insertClosedTaskTable(Word.Document doc, IEnumerable<developerTaskCommitment> taskList)
        {
            var table = addTableToDocument(doc, 9);
            addClosedTaskHeader(table);

            table.Range.Collapse(Word.WdCollapseDirection.wdCollapseEnd);
            var lastPageNumber = table.Range.Information[Word.WdInformation.wdActiveEndPageNumber];
            foreach (developerTaskCommitment taskCommitment in taskList)
            {
                table.Range.Collapse(Word.WdCollapseDirection.wdCollapseEnd);
                var currentPageNumber = table.Range.Information[Word.WdInformation.wdActiveEndPageNumber];
                if (currentPageNumber > lastPageNumber)
                {
                    var newTable = addTableToDocument(doc, 9);
                    addClosedTaskHeader(newTable);
                    var rowToMove = table.Rows.Last;
                    rowToMove.Select();
                    WordApp.Selection.Copy();
                    var dummyRow = newTable.Rows.Add();
                    newTable.Rows[2].Select();
                    WordApp.Selection.Paste();
                    rowToMove.Delete();
                    dummyRow.Delete();
                    table = newTable;
                    currentPageNumber = table.Range.Information[Word.WdInformation.wdActiveEndPageNumber];
                }
                lastPageNumber = currentPageNumber;
                addClosedTaskRow(taskCommitment, table);
            }
        }

        // add a row representing a projected task commitment to a projected task table
        private static void addProjectedTaskRow(developerTaskCommitment dtc, Word.Table table)
        {
            Word.Row row = table.Rows.Add();
            int rowIndex = row.Index;
            fillCol(rowIndex, 1, table, dtc.taskId);
            fillCol(rowIndex, 2, table, "Task");
            fillCol(rowIndex, 3, table, dtc.taskTitle);
            fillCol(rowIndex, 4, table, dtc.taskState);
            fillCol(rowIndex, 5, table, dtc.originalEstimate);
            if ((dtc.isGeneratedPrecedingTask == true) || (dtc.taskState == "Closed"))
            {
                fillCol(rowIndex, 6, table, dtc.remainingWork);
                fillCol(rowIndex, 7, table, dtc.completedWork);
            }
            else
            {
                fillCol(rowIndex, 6, table, dtc.projectedRemainingWork + dtc.projectedCompletedWork);
                fillCol(rowIndex, 7, table, dtc.projectedCompletedWork);
            }
            fillCol(rowIndex, 8, table, dtc.committedDeveloper.developerName);
            Console.WriteLine("Row {0} has printed successfully", rowIndex);
        }

        // add header to a projected task table 
        private static void addProjectedTaskHeader(Word.Table table)
        {
            fillCol(1, 1, table, "Id");
            fillCol(1, 2, table, "Work Item Type");
            fillCol(1, 3, table, "Title");
            fillCol(1, 4, table, "State");
            fillCol(1, 5, table, "Original Estimate");
            fillCol(1, 6, table, "Remaining Hours");
            fillCol(1, 7, table, "Projected Hours To Complete This Week");
            fillCol(1, 8, table, "Assigned To");
            table.Rows[1].HeadingFormat = -1;
            Console.WriteLine("Header row has printed successfully");
        }

        // create a projected task table in target word doc to display projected tasks
        // this table type is also used to display already completed tasks in the current iteration
        private static void insertProjectedTaskTable(Word.Document doc, IEnumerable<developerTaskCommitment> taskList)
        {
            var table = addTableToDocument(doc, 8);
            addProjectedTaskHeader(table);

            table.Range.Collapse(Word.WdCollapseDirection.wdCollapseEnd);
            var lastPageNumber = table.Range.Information[Word.WdInformation.wdActiveEndPageNumber];
            foreach (developerTaskCommitment taskCommitment in taskList)
            {
                table.Range.Collapse(Word.WdCollapseDirection.wdCollapseEnd);
                var currentPageNumber = table.Range.Information[Word.WdInformation.wdActiveEndPageNumber];
                if (currentPageNumber > lastPageNumber)
                {
                    var newTable = addTableToDocument(doc, 8);
                    addProjectedTaskHeader(newTable);
                    var rowToMove = table.Rows.Last;
                    rowToMove.Select();
                    WordApp.Selection.Copy();
                    var dummyRow = newTable.Rows.Add();
                    newTable.Rows[2].Select();
                    WordApp.Selection.Paste();
                    rowToMove.Delete();
                    dummyRow.Delete();
                    table = newTable;
                    currentPageNumber = table.Range.Information[Word.WdInformation.wdActiveEndPageNumber];
                }
                lastPageNumber = currentPageNumber;
                addProjectedTaskRow(taskCommitment, table);
            }
        }

        // given a user story and a list of tasks, outputs those tasks in a closed task formatted table
        private static void outputClosedUserStoryTasks(userStory userStory,
                                            List<developerTaskCommitment> storyTaskCommitments,
                                            Word.WdColor fontColor = Word.WdColor.wdColorAutomatic)
        {
            insertUserStoryName(PreviousIterationTimeLineDoc, userStory, fontColor);
            insertClosedTaskTable(PreviousIterationTimeLineDoc, storyTaskCommitments);
        }

        // given a user story and a list of tasks, outputs those tasks in a projected task formatted table
        private static void outputProjectedUserStoryTasks(userStory userStory,
                                            List<developerTaskCommitment> storyTaskCommitments,
                                            Word.WdColor fontColor = Word.WdColor.wdColorAutomatic)
        {
            insertUserStoryName(CurrentIterationTimeLineDoc, userStory, fontColor);
            insertProjectedTaskTable(CurrentIterationTimeLineDoc, storyTaskCommitments);
        }

        // holds the different lists to be output for an iteration 
        private class IterationContents
        {
            public string iterationPath;
            public string startDate;
            public string endDate;
            public List<developerTaskCommitment> closedTasks;
            public List<developerTaskCommitment> scheduledButNotClosedTasks;
            public List<developerTaskCommitment> assignedActiveAndNewTasks;
        }

        // outputs summary verbiage and info by iteration for individual projects
        private static void outputFeatureSummary(
                feature feature,
                List<developerTaskCommitment> tasks)
        {
            var currentIteration = engine.getCurrentIteration;
            var currentIterationWeek = engine.getCurrentIterationWeek;
            var allIterations = engine.getAllIterations;
            var priorIterations =
                allIterations
                .Where(i => i.endDate <= currentIteration.startDate)
                .OrderBy(i => i.endDate)
                .ToList();
            var immediatelyPriorIteration =
                priorIterations.Last();

            var iterationsThusFar =
                priorIterations
                .ToList();
            iterationsThusFar.Add(currentIteration);

            // output history of iterations thus far (excluding current)
            iterationsThusFar
            .ForEach(i =>
                    {
                        var iterationClosedTasksGroupedByDeveloper =
                            tasks
                            .Where(t => t.committedIteration == i)
                            .GroupBy(t => t.committedDeveloper.developerName)
                            .ToList();

                        if (iterationClosedTasksGroupedByDeveloper.Count > 0)
                        {
                            outputIterationName(PreviousIterationTimeLineDoc, i.path, true, Word.WdColor.wdColorRed);

                            var textToOutput = 
                                iterationClosedTasksGroupedByDeveloper
                                .Aggregate("", ((acc, g) => acc + g.Key + ":\t\t" + g.Sum(t => t.completedWork).ToString() + " hours\n"));

                            outputParagraphText(PreviousIterationTimeLineDoc,textToOutput);
                        }

                        var iterationNotes =
                            feature.iterationNotes
                            .Where(n => n.IterationPath == i.path)
                            .OrderByDescending(n => {
                                var dueDate = n.Fields["Due Date"].Value.ToString();
                                return DateTime.Parse(dueDate);
                             })
                            .ToList();

                        if (iterationNotes.Count > 0)
                        {
                            var textToOutput =
                                iterationNotes
                                .Aggregate("", ((acc, n) => "<p>" + acc + n.Title + "\t\t" + n.Fields["Due Date"].Value.ToString() + "</p>" +
                                                            n.Description));

                            outputParagraphTextHtml(PreviousIterationTimeLineDoc, textToOutput);
                        }
                    });

        }

        // adds a header for the single Feature Status Summary table
        private static void addFeatureStatusSummaryHeader(Word.Table table)
        {
            fillCol(1, 1, table, "Feature");
            fillCol(1, 2, table, "Started On");
            fillCol(1, 3, table, "Ballpark Completion Date");
            fillCol(1, 4, table, "Original Target Completion Date");
            fillCol(1, 5, table, "Current Projected Completion Date");
            fillCol(1, 6, table, "Number of User Stories Fully Tasked");
            fillCol(1, 7, table, "Number of Task Hours Completed");
            fillCol(1, 8, table, "Number of Tasks Behind Schedule by 200% +");
            fillCol(1, 9, table, "Original Estimated Man-Hours");
            fillCol(1, 10, table, "Current Estimated Man-Hours");
            fillCol(1, 11, table, "Current Actual Man-Hours");
            fillCol(1, 12, table, "Current Remaining Man-Hours");
            fillCol(1, 13, table, "Projected Total Man-Hours");
            table.Rows[1].HeadingFormat = -1;
            Console.WriteLine("Summary header row has printed successfully");
        }

        // adds a row for an individual feature to the Feature Status Summary table
        private static void addFeatureStatusSummaryRow(Word.Table table,
            Tuple<feature,
                List<developerTaskCommitment>> feature)
        {
            // this is the beginning of the earliest iteration that has tasks
            var tasksWithScheduledHours =
                feature.Item2
                .Where(t => !((t.hoursAgainstBudget == 0.0) && (!(t.taskState == "Closed"))))
                .ToList();

            var distinctTasks =
                tasksWithScheduledHours
                .GroupBy(t => t.taskId)
                .Select(g => g.First())
                .ToList();

            DateTime startDate =
                feature.Item2
                .Min(t => t.committedIteration.startDate.Value);

            var projectedDateCount = feature.Item1.projectedDates.Length;

            // First projected completion will be entered when feature
            // is first kicked off; this is our "Ballpark Completon Date"
            var initialBallparkCompletionDateItem =
                feature.Item1.projectedDates
                .FirstOrDefault();

            var initialBallparkCompletionDate =
                (initialBallparkCompletionDateItem == default(WorkItem))
                ? ""
                : DateTime.Parse(
                    initialBallparkCompletionDateItem
                    .Fields["Due Date"].Value.ToString()).ToShortDateString();

            // Second projected completion will be entered when feature
            // is fully tasked; this is our "Original Target Completion Date"
            var originalTargetCompletionDateItem =
                feature.Item1.projectedDates
                .Skip(1)
                .FirstOrDefault();

            var originalTargetCompletionDate =
                (originalTargetCompletionDateItem == default(WorkItem))
                ? ""
                : DateTime.Parse(
                    originalTargetCompletionDateItem
                    .Fields["Due Date"].Value.ToString()).ToShortDateString();

            var originalTargetCompletionDateChangeDate =
                (originalTargetCompletionDate == "")
                ? default(DateTime)
                : originalTargetCompletionDateItem
                  .ChangedDate;

            // this is the date from the last projectedDate record
            // this is our "Current Projected Completion Date"
            var currentProjectedCompletionDateItem =
                feature.Item1.projectedDates
                .LastOrDefault();

            var currentProjectedCompletionDate =
                (currentProjectedCompletionDateItem == default(WorkItem))
                ? ""
                : DateTime.Parse(
                    currentProjectedCompletionDateItem
                    .Fields["Due Date"].Value.ToString()).ToShortDateString();


            // used as denominator for "Number of User Stories Fully Tasked"
            int numberOfTotalUserStories =
                feature.Item1.userStories.Length;

            // component of "Number of User Stories Fully Tasked"
            int numberOfUserStoriesWithUnfinishedAuditItems =
                feature.Item2
                .Where(t => t.committedTask.task.Title.ToLower().Contains("audit")
                            && (t.taskState != "Closed"))
                .Select(t => t.parentUserStory)
                .GroupBy(s => s.userStory.Title)
                .Select(g => g.First())
                .Count();


            // component of "Number of User Stories Fully Tasked"
            // if a user story has no child user stories and no child tasks
            // it is added to the sum
            var numberOfUserStoriesWithoutChildren =
                feature.Item1
                .userStories
                .Aggregate(0, (acc, us) =>
                             acc + ((engine.getWorkItemChildUserStories(us.userStory).Count() +
                                     engine.getWorkItemChildTasks(us.userStory).Count() == 0) ? 1 : 0));


            // used as numerator for "Number of User Stories Fully Tasked"
            int numberOfUserStoriesFullyDesigned =
                numberOfTotalUserStories -
                    (numberOfUserStoriesWithUnfinishedAuditItems +
                    numberOfUserStoriesWithoutChildren);

            // used as denominator for "Number of Task Hours Completed" 
            double totalHoursEstimated =
                distinctTasks
                .Sum(t => (t.remainingWork + t.completedWork));

            // used as numerator for "Number of Task Hours Completed"
            double totalHoursCompleted =
                distinctTasks
                .Sum(t => t.completedWork);

            // used as denominator for "Number of Tasks Behind Schedule by 200% +"
            int totalNumberOfTasks =
                distinctTasks.Count();

            // used as numerator for "Number of Tasks Behind Schedule by 200% +"
            int numberOfTasksBehindScheduleByFactorOfTwoOrGreater =
                distinctTasks
                .Where(t => t.completedWork + t.remainingWork
                            >= (t.originalEstimate * 2))
                .Count();

            // component of "Original Estimated Man-Hours"
            double totalCompletedAtTimeOfFirstProjectedDate =
                (originalTargetCompletionDateChangeDate == default(DateTime))
                ? 0.0
                : distinctTasks
                  .Sum(t =>
                  {
                      var lastChange = t.committedTask
                                        .completedChanges
                                        .Where(c => c.changedDate <= originalTargetCompletionDateChangeDate)
                                        .LastOrDefault();
                      return (lastChange == null)
                      ? 0.0
                      : (double)lastChange.postChangeValue;
                  });

            // component of "Original Estimated Man-Hours"
            double totalRemainingAtTimeOfFirstProjectedDate =
                (originalTargetCompletionDateChangeDate == default(DateTime))
                ? 0.0
                : distinctTasks
                  .Sum(t =>
                       { var lastChange = t.committedTask
                                           .remainingChanges
                                           .Where(c => c.changedDate <= originalTargetCompletionDateChangeDate)
                                           .LastOrDefault();
                          return (lastChange == null)
                          ? 0.0
                          : (double) lastChange.postChangeValue;
                        });

            // calculation for "Original Estimated Man-Hours"
            double totalEstimatedHoursAtTimeOfFirstProjectedDate =
                totalCompletedAtTimeOfFirstProjectedDate +
                totalRemainingAtTimeOfFirstProjectedDate;

            // component of "Current Estimated Man-Hours"
            double totalEstimatedHoursForNewTasksSinceTimeOfFirstProjectedDate =
                (originalTargetCompletionDateChangeDate == default(DateTime))
                ? 0.0
                : distinctTasks
                  .Where(t => (t.committedTask
                               .originalEstimateChanges
                               .Min(c => c.changedDate)) > originalTargetCompletionDateChangeDate)
                  .Sum(t => (double)t.committedTask
                                     .originalEstimateChanges
                                     .FirstOrDefault()
                                     .postChangeValue);

            // calculation for "Current Estimated Man-Hours"
            double totalCurrentEstimatedManHours =
                totalEstimatedHoursAtTimeOfFirstProjectedDate +
                    totalEstimatedHoursForNewTasksSinceTimeOfFirstProjectedDate;

            // calculation for "Current Actual Man-Hours"
            double totalCompletedAsOfNow =
                distinctTasks
                .Sum(t =>
                    { var lastChange = t.committedTask
                                       .completedChanges
                                       .LastOrDefault();
                      return (lastChange == null)
                      ? 0.0
                      : (double)lastChange.postChangeValue;
                    });

            double totalRemainingAsOfNow =
                distinctTasks
                .Sum(t =>
                {
                    var lastChange = t.committedTask
                                     .remainingChanges
                                     .LastOrDefault();
                    return (lastChange == null)
                    ? 0.0
                    : (double)lastChange.postChangeValue;
                });

            // calculation for "Projected Total Man-Hours"
            double totalProjectedAsOfNow =
                totalCompletedAsOfNow +
                    totalRemainingAsOfNow;


            //fillCol(1, 1, table, startDate);
            Word.Row row = table.Rows.Add();
            int rowIndex = row.Index;
            fillCol(rowIndex, 1, table, feature.Item1.feature.Title);
            fillCol(rowIndex, 2, table, startDate.ToShortDateString());
            fillCol(rowIndex, 3, table, initialBallparkCompletionDate);
            fillCol(rowIndex, 4, table, originalTargetCompletionDate);
            fillCol(rowIndex, 5, table, currentProjectedCompletionDate);
            fillCol(rowIndex, 6, table, String.Format("{0}/{1}",
                numberOfUserStoriesFullyDesigned, numberOfTotalUserStories));
            fillCol(rowIndex, 7, table, String.Format("{0}/{1}",
                totalHoursCompleted, totalHoursEstimated));
            fillCol(rowIndex, 8, table, numberOfTasksBehindScheduleByFactorOfTwoOrGreater);
            fillCol(rowIndex, 9, table, totalEstimatedHoursAtTimeOfFirstProjectedDate);
            fillCol(rowIndex, 10, table, totalCurrentEstimatedManHours);
            fillCol(rowIndex, 11, table, totalCompletedAsOfNow);
            fillCol(rowIndex, 12, table, totalRemainingAsOfNow);
            fillCol(rowIndex, 13, table, totalProjectedAsOfNow);
            Console.WriteLine("Summary row has printed successfully");
        }

        // outputs a Word Table with one row for each feature and several columns of summary metrics
        private static void outputFeatureSummaryGrid(
                List<Tuple<feature,
                List<developerTaskCommitment>>> features)
        {
            var table = addTableToDocument(TimeLineSummaryDoc,13);
            addFeatureStatusSummaryHeader(table);

            features
            .ForEach(f => addFeatureStatusSummaryRow(table, f));
        }

        // outputs summary of developer hours for each iteration for a given feature
        private static void addFeatureIterationSummary(
            IDictionary<string,developer> developers,
            Tuple<feature,
                List<developerTaskCommitment>> feature)
        {
            var closedItems =
                feature.Item2.Where(t => t.taskState == "Closed");

            var projectedItems =
                feature.Item2.Where(t => t.projectedCompletedWork > 0.0);

            var actualNonClosedItems =
                feature.Item2.Where(t => t.taskState != "Closed" &&
                                         t.completedWork > 0.0);

            var validateCount = feature.Item2.Count() - (closedItems.Count() + projectedItems.Count()
                                                        + actualNonClosedItems.Count());

            var closedHours = closedItems.Sum(t => t.completedWork);
            var projectedHours = projectedItems.Sum(t => t.projectedCompletedWork);
            var actualNonClosedItemsHours = actualNonClosedItems.Sum(t => t.completedWork);

            outputSubHeading(TimeLineSummaryDoc,feature.Item1.feature.Title);
            var table = addTableToDocument(TimeLineSummaryDoc, developers.Count + 2);

            var knownDevelopers = developers.Values.Where(d => d.developerName != "Resource1");

            // set header columns to developer names
            int colIndex = 1;
            fillCol(1, colIndex++, table, "Iteration");
            foreach (var developer in knownDevelopers)
            {
                fillCol(1, colIndex++, table, developer.developerName);
            }

            fillCol(1, colIndex++, table, "Other");

            // if now is within first six months, we show January - July
            // if now is within last six months, we show June - December
            var startIteration =
                feature.Item2
                .Min(t => t.committedIteration.startDate.Value);

            var endIteration =
                feature.Item2
                .Max(t => t.committedIteration.endDate.Value);

            // now output grid: iteration x developer hours
            var iterations =
                engine.getAllIterations
                .ToList()
                .SkipWhile(it => it.endDate <= startIteration)
                .TakeWhile(it => it.endDate <= endIteration)
                .ToList();

            // we know for sure this exists, so we are safe to use First()
            var otherDevelopers = developers.Values.Where(d => d.developerName == "Resource1").First();

            iterations.ForEach((it) =>
            {
                int j = 1;
                Word.Row row = table.Rows.Add();
                int rowIndex = row.Index;
                fillCol(rowIndex, j++, table, it.path);
                foreach (var developer in knownDevelopers)
                {
                    var hours =
                        feature.Item2
                        .Where(t => ((t.committedDeveloper == developer) &&
                                    (t.committedIteration == it)))
                        .Sum(t => (t.isGeneratedPrecedingTask == true) || (t.taskState == "Closed")
                                    ? t.completedWork
                                    : t.projectedCompletedWork);

                    fillCol(rowIndex, j++, table, hours);
                }

                var otherHours = 
                    feature.Item2
                    .Where(t => ((t.committedDeveloper == otherDevelopers) &&
                                (t.committedIteration == it)))
                    .Sum(t => (t.isGeneratedPrecedingTask == true) || (t.taskState == "Closed")
                                ? t.completedWork
                                : t.projectedCompletedWork);

                fillCol(rowIndex, j++, table, otherHours);

            });
        }

        // outputs a table of total developer hours per iteration for a given feature
        private static void outputFeatureIterationSummaryGrids(
                List<Tuple<feature,
                List<developerTaskCommitment>>> features)
        {
            var developers = engine.getDevelopers;

            features
            .ForEach(f => addFeatureIterationSummary(developers, f));
        }

        private static void outputCurrentIterationFeatureTimeline(
            List<developerTaskCommitment> taskCommitments)
        {
            var currentIteration = engine.getCurrentIteration;
            var currentIterationWeek = engine.getCurrentIterationWeek;

            var currentIterationClosedAndActiveTasks =
               taskCommitments
               .Where(c => (c.taskState == "Closed" ||
                            c.taskState == "Active") &&
                            ((c.committedIteration.path == currentIteration.path) &&
                             (c.committedIterationWeek < currentIterationWeek)))
                .GroupBy(tc => tc.committedIteration.path + ": Week " +
                                    tc.committedIterationWeek.ToString())
               .ToList();

            // those tasks with Resource1 as developer have not yet been
            // assigned to anyone on the team
            var scheduledAssignedNewTasksByIteration =
                taskCommitments
                .Where(tc => (((tc.taskState == "New") &&
                              (tc.committedTask.scheduled == true) &&
                              (tc.committedDeveloper.developerName != "Resource1"))) ||
                              ((tc.taskState == "Active") &&
                                    (tc.committedIterationWeek >= currentIterationWeek)))
                .GroupBy(tc => tc.committedIteration.path + ": Week " +
                                    tc.committedIterationWeek.ToString())
                .ToList();

            outputIterationName(CurrentIterationTimeLineDoc,"Projected Iterations Going Forward", false, Word.WdColor.wdColorGreen);

            if ((currentIterationClosedAndActiveTasks.Count() + scheduledAssignedNewTasksByIteration.Count()) == 0)
                outputSubHeading(CurrentIterationTimeLineDoc, "There are no remaining tasks going forward");
            else
            {
                currentIterationClosedAndActiveTasks
                .ForEach(iw =>
                {
                    var closedTasksGroupedByUserStory =
                        iw.GroupBy(ct => ct.parentUserStory)
                        .ToList();

                    outputSubHeading(CurrentIterationTimeLineDoc, iw.Key, Word.WdColor.wdColorPlum);
                    closedTasksGroupedByUserStory
                    .ForEach(g => outputProjectedUserStoryTasks(g.Key, g.ToList(), Word.WdColor.wdColorPlum));
                });

                scheduledAssignedNewTasksByIteration
                .ForEach(iw =>
                {
                    var newAndActiveTasksGroupedByUserStory =
                        iw.GroupBy(ct => ct.parentUserStory)
                        .ToList();

                    outputSubHeading(CurrentIterationTimeLineDoc, iw.Key, Word.WdColor.wdColorGreen);
                    newAndActiveTasksGroupedByUserStory
                    .ForEach(g => outputProjectedUserStoryTasks(g.Key, g.ToList(), Word.WdColor.wdColorGreen));
                });
            }

        }

        private static void outputPreviousIterationFeatureTimeline(List<developerTaskCommitment> taskCommitments)
        {
            var currentIteration = engine.getCurrentIteration;
            var currentIterationWeek = engine.getCurrentIterationWeek;
            var allIterations = engine.getAllIterations;
            var priorIterations =
                allIterations
                .Where(i => i.endDate <= currentIteration.startDate)
                .OrderBy(i => i.endDate);
            var immediatelyPriorIteration =
                priorIterations.Last();

            // we want to report on previous iterations differently from current and future iterations
            // we should show all completed tasks and we should also show tasks originally scheduled for the
            // immediate prior iteration that were not completed

            var priorToCurrentIterationClosedTasksGroupedByIteration =
               taskCommitments
               .Where(c => c.taskState == "Closed" &&
                           priorIterations
                           .Select(i => i.path)
                           .Contains(c.iterationCompleted))
               .GroupBy(c => c.iterationCompleted)
               .OrderByDescending(g => g.Key)
               .ToList();

            var scheduledInImmediatelyPriorIterationButNotClosedTasks =
                taskCommitments
                .Where(c => c.committedTask
                            .iterationChanges
                            .Any(fc =>
                                    {
                                        var preChangeValue = fc.preChangeValue.ToString();
                                        return preChangeValue == immediatelyPriorIteration.path;
                                    }))
                .GroupBy(c => c.committedTask.task.Id)
                .Select(g => g.Last())
                .ToList();

            var priorIterationContents =
                priorIterations
                .Select(i =>
                {
                    var closedTasks = 
                        priorToCurrentIterationClosedTasksGroupedByIteration
                        .FirstOrDefault(g => g.Key == i.path);
                    var scheduledButNotClosedTasks = (i == immediatelyPriorIteration)
                                                        ? scheduledInImmediatelyPriorIterationButNotClosedTasks
                                                        : null;
                    return new IterationContents()
                    {
                        iterationPath = i.path,
                        startDate = i.startDate.Value.ToLongDateString(),
                        endDate = i.endDate.Value.ToLongDateString(),
                        closedTasks = (closedTasks == null) ? null : closedTasks.ToList(),
                        scheduledButNotClosedTasks =
                            (scheduledButNotClosedTasks == null) ? null : scheduledButNotClosedTasks.ToList()
                    };
                })
                .OrderByDescending(i => i.startDate)
                .ToList();

            priorIterationContents
            .FindAll(ic => (ic.closedTasks != null) || (ic.scheduledButNotClosedTasks != null))
            .OrderByDescending(i => i.iterationPath)
            .ToList()
            .ForEach(ic =>
            {
                var closedTasksGroupedAndSortedByUserStory =
                    ic.closedTasks != null 
                    ? ic.closedTasks.GroupBy(t => t.parentUserStory)
                                  .OrderBy(g => g.Key.sortOrder)
                    : null;

                var scheduledButNotClosedTasksGroupedAndSortedByUserStory =
                    ic.scheduledButNotClosedTasks != null 
                    ? ic.scheduledButNotClosedTasks.GroupBy(t => t.parentUserStory)
                                                .OrderBy(g => g.Key.sortOrder)
                    : null;

                // output iteration Path
                outputIterationName(PreviousIterationTimeLineDoc, ic.iterationPath + " (" + ic.startDate + " - " +
                                                                ic.endDate + ")", false,Word.WdColor.wdColorRed);

                // output scheduled but not closed tasks by User Story
                if (ic.scheduledButNotClosedTasks != null)
                {
                    outputSubHeading(PreviousIterationTimeLineDoc, "Tasks Shifted To Next Iteration", Word.WdColor.wdColorPlum);
                    scheduledButNotClosedTasksGroupedAndSortedByUserStory
                    .ToList()
                    .ForEach(g => outputProjectedUserStoryTasks(g.Key, g.ToList(), Word.WdColor.wdColorPlum));
                }

                // output closed tasks by User Story
                if (ic.closedTasks != null)
                {
                    outputSubHeading(PreviousIterationTimeLineDoc, "Tasks Closed During Iteration", Word.WdColor.wdColorRed);
                    closedTasksGroupedAndSortedByUserStory
                    .ToList()
                    .ForEach(g => outputClosedUserStoryTasks(g.Key, g.ToList(), Word.WdColor.wdColorRed));
                }
            }); 
        }

        //For Word 2002 and Word 2003 only
        private static Word.WdBorderType verticalBorder = Word.WdBorderType.wdBorderVertical;
        private static Word.WdBorderType leftBorder = Word.WdBorderType.wdBorderLeft;
        private static Word.WdBorderType rightBorder = Word.WdBorderType.wdBorderRight;
        private static Word.WdBorderType topBorder = Word.WdBorderType.wdBorderTop;
        private static Word.WdBorderType bottomBorder = Word.WdBorderType.wdBorderBottom;

        private static Word.WdLineStyle doubleBorder = Word.WdLineStyle.wdLineStyleDouble;
        private static Word.WdLineStyle noBorder = Word.WdLineStyle.wdLineStyleNone;
        private static Word.WdLineStyle singleBorder = Word.WdLineStyle.wdLineStyleSingle;

        private static Word.WdTextureIndex noTexture = Word.WdTextureIndex.wdTextureNone;
        private static Word.WdColor gray10 = Word.WdColor.wdColorGray10;
        private static Word.WdColor gray70 = Word.WdColor.wdColorGray70;
        private static Word.WdColorIndex white = Word.WdColorIndex.wdWhite;

        private static Word.Style CreateTableStyle(Word.Document doc)
        {
            object styleTypeTable = Word.WdStyleType.wdStyleTypeTable;
            Word.Style styl = doc.Styles.Add
                 ("New Table Style", ref styleTypeTable);
            styl.Font.Name = "Arial";
            styl.Font.Size = 11;
            Word.TableStyle stylTbl = styl.Table;
            stylTbl.Borders.Enable = 1;

            Word.ConditionalStyle evenRowBanding =
                stylTbl.Condition(Word.WdConditionCode.wdEvenRowBanding);
            evenRowBanding.Shading.Texture = noTexture;
            evenRowBanding.Shading.BackgroundPatternColor = gray10;
            // Borders have to be set specifically for every condition.
            evenRowBanding.Borders[leftBorder].LineStyle = doubleBorder;
            evenRowBanding.Borders[rightBorder].LineStyle = doubleBorder;
            evenRowBanding.Borders[verticalBorder].LineStyle = singleBorder;

            Word.ConditionalStyle firstRow =
                stylTbl.Condition(Word.WdConditionCode.wdFirstRow);
            firstRow.Shading.BackgroundPatternColor = gray70;
            firstRow.Borders[leftBorder].LineStyle = doubleBorder;
            firstRow.Borders[topBorder].LineStyle = doubleBorder;
            firstRow.Borders[rightBorder].LineStyle = doubleBorder;
            firstRow.Font.Size = 14;
            firstRow.Font.ColorIndex = white;
            firstRow.Font.Bold = 1;

            // Set the number of rows to include in a "band".
            stylTbl.RowStripe = 1;
            return styl;
        }

        private static void FormatAllTables(Word.Document doc, Word.Style styl)
        {
            foreach (Word.Table tbl in doc.Tables)
            {
                object objStyle = styl;
                tbl.Range.set_Style(ref objStyle);
                // If the table ends in an "even band" the border will
                // be missing, so in this case add the border.

                if (SqlInt32.Mod(tbl.Rows.Count, 2) != 0)
                {
                    tbl.Borders[bottomBorder].LineStyle = doubleBorder;
                }
            }
        }

        private static ProjectTimelineGeneratorEngine.TaskIterationLayoutEngine engine =  new ProjectTimelineGeneratorEngine.TaskIterationLayoutEngine();

        [DataContract]
        public class QueryData
        {
            [DataMember]
            public string query;
        }

        [DataContract]
        internal class QueryAttributes
        {
            [DataMember]
            public string referenceName;
            [DataMember]
            public string name;
            [DataMember]
            public string url;
        }

        [DataContract]
        internal class QueryResults
        {
            [DataMember]
            public string queryType;
            [DataMember]
            public string queryResultType;
            [DataMember]
            public string asOf;
            [DataMember]
            public List<QueryAttributes> columns;
            [DataMember]
            public List<WorkItemResults> workItems;
        }

        [DataContract]
        internal class WorkItemResults
        {
            [DataMember]
            public string id;
            [DataMember]
            public string url;
        }

        static async void PostHttpTest(string username, string password)
        {
            string wiQuery = "Select [System.Id] From WorkItems Where [System.WorkItemType] = 'Bug' And [System.Title] Contains 'CRM CASE XXXX' ";
            try
            {
                using (HttpClient client = new HttpClient())
                {
                    client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
                    client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(Encoding.ASCII.GetBytes(string.Format("{0}:{1}", (object)username, (object)password))));
                    QueryData qData = new QueryData();
                    qData.query = wiQuery;
                    MemoryStream stream1 = new MemoryStream();
                    DataContractJsonSerializer ser = new DataContractJsonSerializer(typeof(QueryData));
                    ser.WriteObject((Stream)stream1, (object)qData);
                    stream1.Position = 0L;
                    StreamReader sr = new StreamReader((Stream)stream1);
                    string wiQueryDataString = sr.ReadToEnd();
                    HttpContent wiQueryDataContent = (HttpContent)new StringContent(wiQueryDataString, Encoding.UTF8, "application/json");
                    string Url = "http://tfs.aiscorp.com:8080/tfs/DefaultCollection/Version 8/_apis/wit/wiql?api-version=1.0";
                    QueryResults qResults;
                    using (HttpResponseMessage httpResponseMessage = await client.PostAsync(Url, wiQueryDataContent))
                    {
                        httpResponseMessage.EnsureSuccessStatusCode();
                        Stream x = await httpResponseMessage.Content.ReadAsStreamAsync();
                        ser = new DataContractJsonSerializer(typeof(QueryResults));
                        qResults = (QueryResults)ser.ReadObject(x);
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }



        static void Main(string[] args)
        {
            //PostHttpTest("mclagett", "06Glendasue15!");

            WordApp = new Word.Application();
            CurrentIterationTimeLineDoc = createDocument();
            CurrentIterationTimeLineDoc.PageSetup.Orientation = Word.WdOrientation.wdOrientLandscape;
            PreviousIterationTimeLineDoc = createDocument();
            PreviousIterationTimeLineDoc.PageSetup.Orientation = Word.WdOrientation.wdOrientLandscape;

            //engine.addInitialBallparkProjection("Site-to-Site Process Portability ");

            var featureTimelines = 
                engine
                .getFeatureTimelines
                .Select(ft => 
                            new Tuple<feature,
                                List<developerTaskCommitment>>
                                    (ft.Item1,ft.Item2.ToList()))
                .OrderBy(ft => ft.Item1.feature.Title)
                .ToList();

            //featureTimelines
            //.ForEach(ft =>
            //{
            //    outputFeatureName(ft.Item1.feature.Title);
            //    outputFeatureSummary(ft.Item1, ft.Item2);
            //});

            featureTimelines
                .ForEach(ft => 
                {
                    outputFeatureName(CurrentIterationTimeLineDoc,ft.Item1.feature.Title);
                    outputCurrentIterationFeatureTimeline(ft.Item2);
                });

            featureTimelines
                .ForEach(ft =>
                {
                    outputFeatureName(PreviousIterationTimeLineDoc,ft.Item1.feature.Title);
                    outputPreviousIterationFeatureTimeline(ft.Item2);
                });

            Word.Style style = CreateTableStyle(CurrentIterationTimeLineDoc);
            FormatAllTables(CurrentIterationTimeLineDoc, style);
            style = CreateTableStyle(PreviousIterationTimeLineDoc);
            FormatAllTables(PreviousIterationTimeLineDoc, style);

            var fileName =
                @"c:\projectTimelineFiles\CurrentIterationProjectTimeline_" +
                DateTime.Now.ToShortDateString().Replace('/', '_') + "_" +
                DateTime.Now.ToShortTimeString().Replace(':', '_').Replace(" ", "_") + ".docx";

            CurrentIterationTimeLineDoc.SaveAs2(fileName);
            CurrentIterationTimeLineDoc.Close();

            fileName =
                @"c:\projectTimelineFiles\PreviousIterationProjectTimeline_" +
                DateTime.Now.ToShortDateString().Replace('/', '_') + "_" +
                DateTime.Now.ToShortTimeString().Replace(':', '_').Replace(" ", "_") + ".docx";

            PreviousIterationTimeLineDoc.SaveAs2(fileName);
            PreviousIterationTimeLineDoc.Close();

            TimeLineSummaryDoc = createDocument();
            TimeLineSummaryDoc.PageSetup.Orientation = Word.WdOrientation.wdOrientLandscape;

            var fileName2 =
                @"c:\projectTimelineFiles\ProjectSummary_" +
                DateTime.Now.ToShortDateString().Replace('/', '_') + "_" +
                DateTime.Now.ToShortTimeString().Replace(':', '_').Replace(" ", "_") + ".docx";

            outputFeatureSummaryGrid(featureTimelines);
            outputFeatureIterationSummaryGrids(featureTimelines);

            style = CreateTableStyle(TimeLineSummaryDoc);
            FormatAllTables(TimeLineSummaryDoc, style);

            TimeLineSummaryDoc.SaveAs2(fileName2);
            TimeLineSummaryDoc.Close();


        }
    }
}
