# ABAP-Hudson
abapGit version of https://blogs.sap.com/2013/05/23/continuous-integration-automated-abap-unit-tests-in-hudson/ 
Copyright Adam Krawczyk

sandraros :

* I took the SAPlink nugget and converted it into abapGit. The tool was running in 731, and I tried to adapt it to 751 **UNSUCCESSFULLY**.
* You will find many lines with comment "751". I commented out the code which couldn't compile, and the code in implicit enhancements. I tried to reattach the enhancements to the right standard objects, but the changes between 731/751 were too big. I had to create missing Z classes, but left them empty.
* Good luck if you want to continue...


Below is the README from Adam Krawczyk :


ABAP Code folder - contains NUGG file with source code (SAPLink) and implicit enhancements saved manually to text files (search for ZCAGS prefix in SAP code).

ABAP_Hudson_integration_manual - general description of framework and example usage

ABAP_Hudson_code_documentation - analyses and technical details of framework implementation

Hudson_job.txt - file containing configuration of main job

Hudson_job_example_output.txt - example output of job to see what are calls to the system and how file content is checked

Readme.txt - what is what :)

I preserve code property rights as mine, but you can use code for your own or company purposes, mentioning that the source comes from me.

Good luck with your ABAP Unit Tests in Hudson!
Adam Krawczyk

