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

package recide.sanex;

import clojure.java.api.Clojure;
import clojure.lang.Keyword;
import clojure.lang.IPersistentMap;
import clojure.lang.PersistentHashMap;
import clojure.lang.IFn;

public final class Utils {

    static {
        IFn require = Clojure.var("clojure.core","require");
        require.invoke(Clojure.read("recide.sanex"));
    }

    private static IFn deref = Clojure.var("clojure.core","deref");
    private static IFn sanitizeFn = Clojure.var("recide.sanex","sanitize");
    public static IPersistentMap NoopSanitization = (IPersistentMap) deref.invoke(Clojure.var("recide.sanex", "noop-sanitization"));
    public static IPersistentMap DefaultSanitization = (IPersistentMap) deref.invoke(Clojure.var("recide.sanex", "noop-sanitization"));

    /**
     Returns the thread-local sanitization options in effect in the code's current context.
     */
    public static IPersistentMap getCurrentSanitizationLevel() {
        return (IPersistentMap) deref.invoke(Clojure.var("recide.sanex", "*sanitization-level*"));
    }

    /**
     Returns a suppression map suitable for consumption by recide's sanitization sledgehammer
     tools.
     @param suppressData If true, this removes any ex-data outside of recide type information.
     @param suppressCause If true, this removes the cause.
     @param suppressMessage If true, this replaces the message with an empty String.
     @param suppressStack If true, this replaces the stacktrace with an empty one.
     @param suppressRecursively If true, suppress causes recursively.
     */
    public static IPersistentMap createSuppressionMap(Boolean suppressData,
                                                      Boolean suppressCause,
                                                      Boolean suppressMessage,
                                                      Boolean suppressStack,
                                                      Boolean suppressRecursively) {
        return PersistentHashMap.create(Keyword.intern("suppress-data?"), suppressData,
                Keyword.intern("suppress-cause?"), suppressCause,
                Keyword.intern("suppress-message?"), suppressMessage,
                Keyword.intern("suppress-stack?"), suppressStack,
                Keyword.intern("suppress-recursively?"), suppressRecursively);
    }

    /**
     Invokes recide.sanex/sanitize.
     */
    public static Throwable sanitize(Throwable t) {
        return (Throwable) sanitizeFn.invoke(t);
    }

    /**
     Invokes recide.sanex/sanitize.
     @param suppression See {@link #createSuppressionMap(Boolean, Boolean, Boolean, Boolean, Boolean) createSuppressionMap}.
     */
    public static Throwable sanitize(Throwable t, IPersistentMap suppression) {
        return (Throwable) sanitizeFn.invoke(t, suppression);
    }
}
