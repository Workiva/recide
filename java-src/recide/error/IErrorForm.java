// Copyright 2016-2019 Workiva Inc.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package recide.error;

import clojure.lang.Keyword;
import clojure.lang.IFn;
import clojure.lang.APersistentMap;
import java.util.ArrayList;

public interface IErrorForm {
    public String getSerializationTag();
    public Keyword getTypeKeyword();
    public Keyword getMessageKeyword();
    public Keyword getDataKeyword();
    public Keyword getCauseKeyword();
    public Keyword getSerializedKeyword();
    public IFn getConstructor();
    public APersistentMap getMetadataFunctions();
    public ArrayList<APersistentMap> getRaisedSites();
    public void addRaisedSite(APersistentMap m);
}
