## Changelog

### v0.19 - 2020-02-13

- Rewrite gauge chart to add legend

### v0.18 - 2020-02-13

- Add circulation calculation
- Tidy up tables and charts

### v0.17 - 2020-02-11

- Match report to NPS template
- Select meeting rooms by default if NPS, Desk Settings otherwise
- Better error handling

### v0.16 - 2020-02-05

- Fix bottleneck due to na.rm = TRUE
- Add first downloadable report draft
- minor chart tweaks

### v0.15 - 2020-02-04

- Revised the tables to use the correct calculations of space
- All three charting tabs now update with the filters as per request
- Now the filter defaults to selecting the meeting rooms, rather than desk settings, when downloading the app
- Combined the actual/target occupancy gauges into a single gauge
- The app once again loads with a dummy dataset on load, as my previous method caused some issues.
- Made the charts a bit more colourful


### v0.14 - 2020-01-27

- Revised room charts
- Added room filter
- Added lots of tables

### v0.13 - 2020-01-15

- Allow selection of multiple surveys
- Error message if there are bad surveys in survey list
- First draft of NPS charts

### v0.12 - 2019-11-28

- Filter initial data load by the selected time range

### v0.11 - 2019-09-09

- Changes data download method to using dbtools
- Add admin tab for changing survey dropdown list
- Split recommendation tables into longstay and non-longstay
- Add percentages to charts
- remove plotly from charts

### v0.10 - 2019-05-23

- Add filter for buildings

### v0.9 - 2019-02-04

- Adds options for report output by floor, or by floor and team
- Removed slidy report
- Added survey name to report for easier trackability
- Re-enabled email authorisation

### v0.8.1 - 2018-12-12

- Updates survey list at server function

### v0.8 - 2018-12-10

- Adds filters for zone and desks
- (hopefully) fixes timeout issue with large Markdown reports

### v0.7.3 - 2018-12-06

- Revert authentication change
- Fix several bugs
- Added latest Home Office surveys

### v0.7.2 - 2018-11-06

Temporary switch to email authentication

### v0.7.1 - 2018-10-12

Minor update to make sure the data updates work correctly.

### v0.7 - 2018-10-11

- Add MoJ template to word report

### v0.6 - 2018-10-09

- Adds disclaimer to Word report (#35)
- Make data download date range end yesterday rather than today (#37)
- Group desk types by roomtype, selecting just Desk Settings by default (#38)
- Fix listener for floor picker, so the filter updates when changing it (#41)
- Add changelog

### v0.5.2 - 2018-10-02

Hotfix to strip whitespace from team filter, to align with the filter tree behaviour

### v0.5.1 - 2018-09-27

Hotfix to correct issue with the daily usage chart narrative, which produced an error if there were no sensors selected (which shouldn't happen) or if there were no unused desks (which occasionally happens)

### v0.5 - 2018-09-27

- Adds the peak occupancy tables to the markdown reports (#23)
- Clarifies the recommendation table text (#24)
- Outputs charts for all teams (#25)
- Adds "key message" to all charts (#26)
- Improves layout of the intro page, using "words in tables" technique (#27)
- Have lightbox that gives proper feedback that the dataset was loaded into the interface (#28)
- Removes the accumulator and the test for <2 30-minute stretches. This test required heavy client-side data crunching, with only marginal impact on report (#29)
- Default data download date range to start a month before today. (#30)

### v0.4 - 2018-09-05

- Survey list filtered by a list of active surveys, hosted in S3 to remove need for redeploying when updating.
- You can now choose a range of dates for which to download the data. The concept is that the top half of the Report config tab, for downloading the data to the interface, is the slow run-once part, while the filters can update much quicker once the data’s there. However, previously it downloaded and summarised the full dataset, which could take several minutes for the particularly large datasets (since it contains several months of data). If you only download the date range you need (say, a month’s worth, for monthly reporting), it should download in a reasonable time. I’ll keep an eye out for further areas for potential improvement.
- The desk type(s) and floor(s) picker is now sorted correctly
- I’ve moved the Smoothing Factor control to the Smoothing tab and added some description to explain what the chart is saying.
- I’ve added the expanded description to the allocation strategy table in summary tables
- Also on the summary tables tab, I’ve added a table of the top 10 occupied days.
- I’ve added download buttons to all the raw data tables, so you can download them as CSVs
- I’ve added a table for Filtered data, which shows the data as currently filtered – the Summarised Data tab shows the same without any filter applied (and so includes sensors with bad data)
- I’ve added a Bad Observations table, which shows a summary of sensors by day and number of bad observations that day.

### v0.3.3 - 2018-08-29

Third bugfix for v0.3

### v0.3.2 - 2018-08-28

Second bugfix for v0.3

### v0.3.1 - 2018-08-28

Bugfix for v0.3

### v0.3 - 2018-08-28

- Revise the UI to remove the "update filter" button, and instead update the filter every time you update the filter
- Download the data based on the revised data scheme, wherein the data is stored in Feather format, and only the minimum data is stored - the metadata is brought in by joining with the Sensors table.

### v0.2 - 2018-07-18

First public release
