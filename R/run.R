## Copyright (C) 2020 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

is_gen_done<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    file.exists(fnFlag)
}

is_ms2_done<-function(set,dest) {
    fnFlag<-file.path(dest,paste('.',set,'.DONE',sep=''))
    file.exists(fnFlag)
}

set_ms2_done<-function(set,dest) {
    fnFlag<-file.path(dest,paste('.',set,'.DONE',sep=''))
    file.create(fnFlag)
}

set_gen_done<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    file.create(fnFlag)
}

unset_gen_done<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    if (is_gen_done(dest)) unlink(fnFlag,force=T)
}
