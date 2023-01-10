## Copyright (C) 2020,2021,2023 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

errc_mf_jar_absent = errorCondition("MetFrag jar file specified, but cannot be found.", class = "mf-jar-absent")
errc_mf_db_dir_absent = errorCondition("MetFrag DB directory specified, but cannot be found.", class = "mf-db-dir-absent")
errc_mf_db_file_absent = errorCondition("MetFrag DB file specified, but cannot be found.", class = "mf-db-dir-absent")

errc_projects_absent = errorCondition("User root directory (projects), currently does not exist.. Abort.", class= "projects-absent")
errc_top_data_dir_absent = errorCondition("Data directory (top_data_dir) does not exist. Abort.",
                                         class = "top-data-dir-absent")

errc_conf_file_absent <- errorCondition("There is no config file in the project directory.",class="conf-file-absent")
