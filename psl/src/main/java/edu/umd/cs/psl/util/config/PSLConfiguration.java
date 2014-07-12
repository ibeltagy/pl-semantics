/*
 * This file is part of the PSL software.
 * Copyright 2011 University of Maryland
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package edu.umd.cs.psl.util.config;

import java.io.File;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import com.google.common.base.Preconditions;

import de.mathnbits.config.Configuration;

public class PSLConfiguration extends Configuration {

	public static final long startTime = System.currentTimeMillis();
	public static long timeout = 120000; //2 minutes
	public static int groundLimit = 10000; //grounding limit when using AVG. 0 means: no limit. Take care that "no limit" may not work for long sentences
	public static double metaW = -1; //weight of meta predicates, they are negationPred and dummyPred. "-1" means they should be treated as any other unary predicate.
	public static double relW = -1; //weight of relation predicates. "-1" means they should be treated as any other unary predicate.
	
	public PSLConfiguration(String rootDir, String configFile, String baseFile) {
		super(rootDir+File.separator+configFile,(baseFile==null?null:rootDir+File.separator+baseFile));
	}
	
	public PSLConfiguration(String configFile, String baseFile) {
		this(defaultDir,configFile,baseFile);
	}
	
	public PSLConfiguration(String configFile) {
		this(configFile,null);
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this,ToStringStyle.MULTI_LINE_STYLE);
	}
	
	private static String defaultDir = "./";
		
	public static void setDefaultDir(String dir) {
		Preconditions.checkNotNull(dir);
		File d = new File(dir);
		Preconditions.checkArgument(d.exists());
		Preconditions.checkArgument(d.isDirectory());
		defaultDir = dir;
	}
	
}
