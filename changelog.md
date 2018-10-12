## Changelog

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
