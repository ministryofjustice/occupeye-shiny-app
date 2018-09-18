# OccupEye data dashboard

## Introduction

This is a tool for exploring data extracted from OccupEye sensors deployed by MoJ. In particular, it summarises sensor data into daily observations, under the following categories.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> category </th>
   <th style="text-align:left;"> description </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Unused desks </td>
   <td style="text-align:left;"> An unused desk is a desk which is not occupied for an entire day. To account for potential errors desks have been classified as unused where desk utilisation in the sample period is less than 15%, and there are fewer than 2 occurrences of that desk being occupied for 30-minute stretches during the day. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Under Utilised Desks </td>
   <td style="text-align:left;"> For the purposes of this report an under utilised desk is a used desk which is used for less than 50% of the time during the day. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> In use </td>
   <td style="text-align:left;"> A desk is used for more than 50% of the day. </td>
  </tr>
</tbody>
</table>

A note on Multiple Occupancy: It is not possible within the main data to identify instances where more than one individual has occupied a single desk. However the absence of activity at a desk for an extended period of time does indicate sub-optimal utilisation of desk space.


## Operation

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Instruction </th>
   <th style="text-align:left;"> Detail </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Report Config </td>
   <td style="text-align:left;"> Choose the report from the dropdown that you want to download. Currently the reports are saved as individual datasets grouped by team. For some of the smaller directorates, you can load the whole directorate, while other directorates are split into departments. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Time Interval </td>
   <td style="text-align:left;"> Select the time interval you wish the data to be filtered - by default 09:00 to 17:00. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Load Report </td>
   <td style="text-align:left;"> Hit the Load report button to load the dataset into the interface. This could take a minute or two, as indicated by the loading bars in the bottom-right corner of the screen. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Update the filters </td>
   <td style="text-align:left;"> Once you have loaded the dataset, you can update filters for dates, floors, desk types and teams to include in the reports. The charts and tables will update accordingly. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Download reports </td>
   <td style="text-align:left;"> Once you have configured the filters to your satisfaction, you can generate downloadable reports from the Download Reports tab. You can choose between an HTML-based slideshow, and a Microsoft Word report. These follow the format of the reports produced previously by ASD - first showing charts and tables for the whole dataset, then showing the daily charts for each team (if there are multiple teams selected). </td>
  </tr>
</tbody>
</table>


## Contact

Please direct any questions, suggestions or bug-reports to Thomas Hirsch, thomas.hirsch@justice.gov.uk

