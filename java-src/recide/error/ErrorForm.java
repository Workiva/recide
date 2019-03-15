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

import clojure.java.api.Clojure;
import clojure.lang.Keyword;
import clojure.lang.APersistentMap;
import clojure.lang.IFn;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

public class ErrorForm implements IErrorForm, Serializable {

    private static ConcurrentHashMap<String,ErrorForm> tagToErrorForm = new ConcurrentHashMap<String,ErrorForm>();

    private String serializationTag;
    private Keyword type;
    private Keyword message;
    private Keyword data;
    private Keyword cause;
    private Keyword serialized;
    private IFn constructor;
    private APersistentMap metadataFunctions;
    private ArrayList<APersistentMap> raisedSites;
   
    public ErrorForm(String serializationTag,
		     Keyword type,
		     Keyword message,
		     Keyword data,
		     Keyword cause,
		     Keyword serialized,
		     IFn constructor,
		     APersistentMap metadataFunctions) {
	this.tagToErrorForm.put(serializationTag, this);
	this.serializationTag = serializationTag;
	this.type = type;
	this.message = message;
	this.data = data;
	this.cause = cause;
	this.serialized = serialized;
	this.constructor = constructor;
	this.metadataFunctions = metadataFunctions;
	this.raisedSites = new ArrayList<APersistentMap>();
    }

    @Override
    public String getSerializationTag() {
	return serializationTag;
    }

    @Override
    public Keyword getTypeKeyword() {
	return type;
    }

    @Override
    public Keyword getMessageKeyword() {
	return message;
    }

    @Override
    public Keyword getDataKeyword() {
	return data;
    }

    @Override
    public Keyword getCauseKeyword() {
	return cause;
    }

    @Override
    public Keyword getSerializedKeyword() {
	return serialized;
    }

    @Override
    public IFn getConstructor() {
	return constructor;
    }

    @Override
    public APersistentMap getMetadataFunctions() {
	return metadataFunctions;
    }

    @Override
    public ArrayList<APersistentMap> getRaisedSites() {
	return this.raisedSites;
    }

    @Override
    public void addRaisedSite(APersistentMap m) {
	raisedSites.add(m);
    }

    private void writeObject(ObjectOutputStream oos) throws IOException {
	oos.writeObject(serializationTag);
    }

    private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException {
	this.serializationTag = (String) ois.readObject();
	if (! tagToErrorForm.containsKey(serializationTag)) {
	    throw new ClassNotFoundException("" + serializationTag + " corresponds to no known ErrorForm.");
	}
	ErrorForm myself = tagToErrorForm.get(serializationTag);
	this.type = myself.getTypeKeyword();
	this.message = myself.getMessageKeyword();
	this.data = myself.getDataKeyword();
	this.cause = myself.getCauseKeyword();
	this.serialized = myself.getSerializedKeyword();
	this.constructor = myself.getConstructor();
	this.metadataFunctions = myself.getMetadataFunctions();
	this.raisedSites = myself.getRaisedSites();
    }
}
