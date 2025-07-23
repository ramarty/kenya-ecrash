# Kenya-Police-Dashboard
Dashboard for Kenya police.

# Instructions

## Buttons on Main Page
1. `Generate New Report` opens a new page to enter data for a new crash.
2. `Sync Data with Server` sends data from the computer to the server and sends data from the server to the computer.
3. `Update System' updates the dashboard system. Doing so will cause the system to close so it will need to be reopened.
4. `Edit & Print P41/Sit. Report` opens a report that is still able to be edited.
5. `Print P41/Sit. Report` opens a report that cannot be edited. This page allows downloading the P41 and Situation Report.

## Buttons in Edit & Print P41/Sit. Report
1. `Back to Report List` goes back to the list of all reports
2. `Finalice Form` will not allow users to edit the information anymore; user will only be able to download the P41 and Situation Report.
3. `Delete Form` will delete the crash report.
4. `Save Data` will save any edits made to the crash report.
5. `Download p41 PDF` downloads a PDF of the P41
6. `Generate Situation Report` goes to page with a draft of the Situation Report that can then be edited.

## Buttons in Generate Situation Report
This page will auto-generate a draft situation report based on data entered from the crash report. The automated process of generating the Situation Report is not perfect; consequently, a user can still make and save edits to the Report.
1. `Back to Report List` goes back to the list of all reports
2. `Download Situation Report` downloads a PDF of the situation report.
3. `Save Edits to Situation Report` saves any edits made to the situation report.
4. `Revert Situation Report to Original` disregards any edits made to the situation report and goes back to the auto-generated version of the report.
5. `Back to Data Page` goes back to the data editing page for the report (UNDER DEVELOPMENT / WILL BE ADDED SOON).


# Install
1. Install Github
2. Install R
3. Install RStudio
4. Install Google Chrome
5. Open the Command Prompt
* Type: `cd Documents`
* Type: `git clone https://github.com/ramarty/Kenya-Police-Dashboard.git`
* You will be prompted to enter a username and password. These will be provided to you.
6. Manually put the `api_keys.Rds` and `hashed_passwords.Rds` files into the `Kenya-Police-Dashboard` folder. These files will be sent to you.
7. Go in the `Kenya-Police-Dashboard` folder and double-click `installer.R`. This should open a script in RStudio. (If the script does not open, then open RStudio then double click `installer.R`).
8. In RStudio, click `Source` in the top right corner.
9. In the `Kenya-Police-Dashboard` folder, copy the `shinyShortcut.vbs` file and paste it to the Desktop.
