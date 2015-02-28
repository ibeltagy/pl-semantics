package uk.ac.ed.easyccg.syntax;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ed.easyccg.lemmatizer.Lemmatizer;
import uk.ac.ed.easyccg.lemmatizer.Lemmatizer_MIT;
import uk.ac.ed.easyccg.syntax.Combinator.RuleType;
import uk.ac.ed.easyccg.syntax.SyntaxTreeNode.SyntaxTreeNodeBinary;
import uk.ac.ed.easyccg.syntax.SyntaxTreeNode.SyntaxTreeNodeLeaf;
import uk.ac.ed.easyccg.syntax.SyntaxTreeNode.SyntaxTreeNodeUnary;
import uk.ac.ed.easyccg.syntax.SyntaxTreeNode.SyntaxTreeNodeVisitor;
import uk.ac.ed.easyccg.syntax.Util.Pair;

import com.google.common.base.Strings;

public abstract class ParsePrinter
{
  public final static ParsePrinter CCGBANK_PRINTER = new CCGBankPrinter();
  public final static ParsePrinter HTML_PRINTER = new HTMLPrinter();
  public final static ParsePrinter PROLOG_PRINTER = new PrologPrinter();
  public final static ParsePrinter EXTENDED_CCGBANK_PRINTER = new ExtendedCCGBankPrinter();
  public final static ParsePrinter SUPERTAG_PRINTER = new SupertagPrinter();
  private static Boolean reset = true;
  private static int lastPrintedId = -1;

  public String print(List<SyntaxTreeNode> parses, int id)
  {
    StringBuilder result = new StringBuilder();
    
   if(reset)
	   printFileHeader(result);
   reset = false;
   
    if (parses == null) {
      if (id > -1) printHeader(id, result);
      printFailure(result);
    } else {
      boolean isFirst = true;
      for (SyntaxTreeNode parse : parses) {
        if (isFirst) {
          isFirst = false;
        } else {
          // Separate N-best lists
          result.append("\n");
        }
        if (id > -1) printHeader(id, result);
        printParse(parse, id, result);
      }
    }
    
    printFooter(result);
    return result.toString();
  }
  
  public String print(SyntaxTreeNode entry, int id) {
    StringBuilder result = new StringBuilder();
    if (id > -1) printHeader(id, result);
    
    if (entry == null) {
      printFailure(result);
    } else {
      printParse(entry, id, result);
    }

    
    printFooter(result);
    return result.toString();
  }
  
  public void reset(){
	  reset = true;
	  lastPrintedId = -1;
  }


  abstract void printFileHeader(StringBuilder result);
  abstract void printFailure(StringBuilder result);
  abstract void printHeader(int id, StringBuilder result);
  abstract void printFooter(StringBuilder result);
  abstract void printParse(SyntaxTreeNode parse, int sentenceNumber, StringBuilder result);

  abstract static class ParsePrinterVisitor implements SyntaxTreeNodeVisitor {
    final StringBuilder result;

    private ParsePrinterVisitor(StringBuilder result)
    {
      this.result = result;
    }
    
  }
  
  
  private static class CCGBankPrinter extends ParsePrinter {

    @Override
    void printFailure(StringBuilder result) {
      result.append("(<L NP NN NN fail N>)");
    }

    class CCGBankParsePrinterVisitor extends ParsePrinterVisitor {
      // (<T S[dcl] 0 2> (<T S[dcl] 1 2> (<T NP 0 2> (<T NP 0 2> (<T NP 0 2> (<T NP 0 1> (<T N 1 2> (<L N/N NNP NNP Pierre N_73/N_73>) (<L N NNP NNP Vinken N>) ) ) (<L , , , , ,>) )

      CCGBankParsePrinterVisitor(StringBuilder result) {
        super(result);
      }

      @Override
      public void visit(SyntaxTreeNodeBinary node)
      {
        result.append("(<T ");
        result.append(node.getCategory().toString());
        result.append(" " + (node.headIsLeft ? "0" : "1") + " 2> ");
        node.leftChild.accept(this);
        node.rightChild.accept(this);

        result.append(") ");

      }

      @Override
      public void visit(SyntaxTreeNodeUnary node)
      {
        result.append("(<T ");
        result.append(node.getCategory().toString());
        result.append(" 0 1> ");
        node.child.accept(this);

        result.append(") ");

      }

      @Override
      public void visit(SyntaxTreeNodeLeaf node)
      {
        result.append("(<L ");
        result.append(node.getCategory().toString());
        if (node.getPos() == null) {
          result.append(" POS POS ");
        } else {
          result.append(" " +node.getPos() + " " + node.getPos() + " ");
        }
        result.append(normalize(node.getWord()));
        result.append(" ");
        result.append(node.getCategory().toString());
        result.append(">) ");        
      }
    }

    /**
     * Normalizes words - e.g. converting brackets to CCGBank format.
     */
    private static String normalize(String word)
    {
      if (word.length() > 1) {
        return word;
      } else if (word.equals("{")) {
        return "-LRB-";
      } else if (word.equals("}")) {
        return "-RRB-";
      } else if (word.equals("(")) {
        return "-LRB-";
      } else if (word.equals(")")) {
        return "-RRB-";
      }
      return word;
    }

    @Override
    void printHeader(int id, StringBuilder result)
    {
      result.append("ID=" + id + "\n");
    }

    @Override
    void printFooter(StringBuilder result)
    {
    }

    @Override
    void printParse(SyntaxTreeNode parse, int sentenceNumber, StringBuilder result)
    {
      parse.accept(new CCGBankParsePrinterVisitor(result));
    }

    @Override
    void printFileHeader(StringBuilder result)
    {
    }
  }
  
  private static class HTMLPrinter extends ParsePrinter {
    @Override
    void printFailure(StringBuilder result)
    {
      result.append("<p>Parse failed!</p>");
    }

    @Override
    void printHeader(int id, StringBuilder result)
    {

      result.append("<html>\n" + 
      		"<body>\n");
    }

    @Override
    void printFooter(StringBuilder result)
    {

      result.append("</body>\n" + 
      		"</html>");
    }

    @Override
    void printParse(SyntaxTreeNode parse, int sentenceNumber, StringBuilder result)
    {
      result.append("<table>");
      for (List<SyntaxTreeNode> row : getRows(parse)) {
        result.append("<tr>");
        int indent = 0;
        while (indent < row.size()) {
          SyntaxTreeNode cell = row.get(indent);

          if (cell == null) {
            result.append("<td></td>");
            indent = indent + 1;
          }else if (cell.isLeaf()) {
            result.append(makeCell(((SyntaxTreeNodeLeaf) cell).getWord(), cell.getCategory()));
            indent = indent + 1;
          } else {
            int width =  getWidth(cell);
            result.append(makeCell(cell.getCategory(), width));
            indent = indent + width;
          }
        }
        result.append("</tr>\n");
      }
      result.append("</table>");
    }
    
    List<List<SyntaxTreeNode>> getRows(SyntaxTreeNode parse) {
      List<List<SyntaxTreeNode>> result = new ArrayList<List<SyntaxTreeNode>>();
      getRows(parse, result, 0);
      return result;
    }

    int getWidth(SyntaxTreeNode node) {
      if (node.getChildren().size() == 0) {
        return 1;
      } else {
        int result = 0;
        
        for (SyntaxTreeNode child : node.getChildren()) {
          result = result + getWidth(child); 
        }
        
        return result;
      }
    }
    @Override
    void printFileHeader(StringBuilder result)
    {
    }
    
    int getRows(SyntaxTreeNode node, List<List<SyntaxTreeNode>> result, int minIndentation) {
      
      int maxChildLevel = 0;
      int i = minIndentation;
      for (SyntaxTreeNode child : node.getChildren()) {
        maxChildLevel = Math.max(getRows(child, result, i), maxChildLevel);
        i = i + getWidth(child);
      }
      
      int level;
      if (node.getChildren().size() > 0) {
        level = maxChildLevel + 1;
      } else {
        level = 0;
      }

      while (result.size() < level + 1) {
        result.add(new ArrayList<SyntaxTreeNode>());
      }
      while (result.get(level).size() < minIndentation + 1) {
        result.get(level).add(null);
      }
      
      result.get(level).set(minIndentation, node);
      return level;
    }
    
    private String makeCell(Category category, int width) {
      return "<td colspan=" + width + "><center><hr>" + category.toString() + "</center></td>";
    }
    private String makeCell(String word, Category category) {
      return "<td><center>" + word + "<hr>" + category + "</center></td>";
    }
  }

  public static class SupertagPrinter extends ParsePrinter {

    @Override
    void printFileHeader(StringBuilder result)
    {
    }

    @Override
    void printFailure(StringBuilder result)
    {
    }

    @Override
    void printHeader(int id, StringBuilder result)
    {
    }

    @Override
    void printFooter(StringBuilder result)
    {
    }

    @Override
    void printParse(SyntaxTreeNode parse, int sentenceNumber, StringBuilder result)
    {
      // word|pos|tag word|pos|tag word|pos|tag
      boolean isFirst = true;
      for (SyntaxTreeNodeLeaf word : parse.getWords()) {
        if (isFirst) {
          isFirst = false;
        } else {
          result.append(" ");
        }
        
        result.append(word.getWord() + "|" + (word.getPos() == null ? "" : word.getPos()) + "|" + word.getCategory());
      }
    }
  }
  
   
  public static class PrologPrinter extends ParsePrinter {
	  private static int lastCounter = 0;
	  private static final ArrayList<Combinator> combinatorsList =  new ArrayList<Combinator>(Combinator.STANDARD_COMBINATORS);
    /*
ccg(2,
 ba('S[dcl]',
  lf(2,1,'NP'),
  fa('S[dcl]\NP',
   lf(2,2,'(S[dcl]\NP)/NP'),
   lex('N','NP',
    lf(2,3,'N'))))).

w(2, 1, 'I', 'I', 'PRP', 'I-NP', 'O', 'NP').
w(2, 2, 'like', 'like', 'VBP', 'I-VP', 'O', '(S[dcl]\NP)/NP').
w(2, 3, 'cake', 'cake', 'NN', 'I-NP', 'O', 'N').

     */
    
	private static String formatCategory (Category category) {
		return category.toString().toLowerCase().replace("]", "").replace("[", ":");//.replace("s:b", "s:dcl").replace("s:to", "s:dcl").replace("s:x", "s:dcl").replace("s:pss", "s:dcl").replace("s:ng", "s:dcl");
	}
	
	private static String formatWord(String word) {
		String formatedWord = word.replace("'", "\\'"); 
		return formatedWord ;
	}	
    private static String getRuleName(RuleType combinator) {
      // fa ba fc bx gfc gbx  conj
      switch (combinator) 
      {
      case FA : return "fa";
      case BA : return "ba";
      case FC : return "fc";
      case BX : return "bxc"; //<<<----------bxc
      case GFC : return "gfc";
      case GBX : return "gbxc"; //<<<<<<<<<<gbxc
      case CONJ : return "conj";
      case RP : return "rp";
      }
      
      throw new RuntimeException("Unknown rule type: " + combinator);
    }

    private static int getCombinatorIndex(RuleType combinator) {
    	int index = -1;
    	//FA, BA, FC, BX, GFC, GBX, CONJ, RP, NOISE, UNARY, LEXICON
        switch (combinator) 
        {
        case FA : index = 0; break;
        case BA : index = 1; break;
        case FC : index = 2; break;
        case BX : index = 3; break;
        case GFC : index = 4; break;
        case GBX : index = 5; break;
        case CONJ : index = 6; break;
        case RP : index = 7; break;
        case NOISE : index = -1; break;
        case UNARY : index = -1; break;
        case LEXICON : index = -1; break;
        }
        if (index < 0)
        	throw new RuntimeException("Unknown rule type: " + combinator);

        return index;
      }

    private static Combinator getCombinator(RuleType combinator) {
    	return combinatorsList.get(getCombinatorIndex(combinator));
    }
    
    @Override
    void printFileHeader(StringBuilder result)
    {
      result.append(":- multifile w/8, ccg/2, id/2.\n" + 
      		":- discontiguous w/8, ccg/2, id/2.\n" + 
      		":- dynamic w/8, ccg/2, id/2.\n" + 
      		"\n" + 
      		"");
    }

    
    @Override
    void printFailure(StringBuilder result)
    {
    }

    @Override
    void printHeader(int id, StringBuilder result)
    {
    }

    @Override
    void printFooter(StringBuilder result)
    {
    }

    @Override
    void printParse(SyntaxTreeNode parse, int sentenceNumber, StringBuilder result)
    {
      Combinator.isPrinting = true;
      printDerivation(parse, sentenceNumber, result);
      result.append("\n");

      Combinator.isPrinting = false;
/*      
      int i = 0;
      for (SyntaxTreeNodeLeaf word : parse.getWords()) {
        //w(2, 1, 'I', 'I', 'PRP', 'I-NP', 'I-PER', 'NP').
        i++;
        result.append("w(" + sentenceNumber + ", " + i + ", '" + word.getWord() + "', '" + Lemmatizer.lemmatize(word.getWord(), word.getPos()) + "', '" + word.getPos() + "', 'O" + "', '" + word.getNER()  + "', '" + word.getCategory() + "').\n");
      }
      */
    }

    private void printDerivation(SyntaxTreeNode parse, int id, StringBuilder result)
    {
      if (id != lastPrintedId)
      {
    	  lastPrintedId = id;
    	  lastCounter = 0;
      }
      lastCounter++;
      int extendedId = id*100 + lastCounter;
      result.append("ccg(" + extendedId);
      parse.accept(new DerivationPrinter(result, id));
      result.append(").\n");
      //result.append("id("+1.0/lastCounter+", ["+id+"]).\n");
    }

    private class DerivationPrinter extends ParsePrinterVisitor {
      int currentIndent = 1;
      int wordNumber = 1;
      final int sentenceNumber;
      DerivationPrinter(StringBuilder result, int sentenceNumber) {
        super(result);
        this.sentenceNumber = sentenceNumber;
      }
      
      @Override
      public void visit(SyntaxTreeNodeBinary node)
      {
        // ba('S[dcl]',
        result.append(",\n");
        printIndent(currentIndent);
        //node.leftChild.getCategory().setFeatureStoX();
        //node.rightChild.getCategory().setFeatureStoX();
        //System.out.println(node.getRuleType() + ", " + node.getCategory() + ", " + node.leftChild.getCategory() + ", " + node.rightChild.getCategory() + " >>" );
        Combinator comb = PrologPrinter.getCombinator(node.getRuleType());
        Pair<Category, Category> pair = comb.modifyForBoxer(node.getCategory(), node.leftChild.getCategory(), node.rightChild.getCategory());
        node.leftChild.setCategory(pair.t);
        node.rightChild.setCategory(pair.u);
        RuleType ruleType = node.getRuleType();
        SyntaxTreeNode leftChild = node.leftChild;
        SyntaxTreeNode rightChild = node.rightChild;
    	if(node.ruleType == RuleType.BA 
        		&& node.getCategory().toString().equals("S[ng]\\NP")
        		&& node.leftChild.getCategory().toString().equals("(S[X]\\NP)/NP") 
        		&& node.rightChild.getCategory().toString().equals("(S[ng]\\NP)\\((S[X]\\NP)/NP)")
        		&& node.rightChild instanceof SyntaxTreeNodeUnary )
        	{
    			ruleType = RuleType.FA;
    			leftChild.setCategory(leftChild.getCategory().doSubstitution("ng"));
    			rightChild = ((SyntaxTreeNodeUnary) node.rightChild).child;
        	}
        
        if (ruleType == RuleType.CONJ)
        	result.append(getRuleName(ruleType) + "(" + formatCategory(node.getCategory()) + "," +formatCategory(node.rightChild.getCategory()) + "");
        else
    		result.append(getRuleName(ruleType) + "(" + formatCategory(node.getCategory()));

        currentIndent++;    
        leftChild.accept(this);
        rightChild.accept(this);
        result.append(")");
        currentIndent--;
      }

      @Override
      public void visit(SyntaxTreeNodeUnary node)
      {
        // lex('N','NP',
        result.append(",\n");
        printIndent(currentIndent);
        result.append("lx(" + formatCategory(node.getCategory()) + "," +formatCategory(node.child.getCategory()) + "");
        currentIndent++;        
        node.child.accept(this);
        result.append(")");
        currentIndent--;
      }
      
      @Override
      public void visit(SyntaxTreeNodeLeaf node)
      {
        //   lf(2,2,'(S[dcl]\NP)/NP'),
        result.append(",\n");
        printIndent(currentIndent);
        //result.append("t(" + sentenceNumber + "," + wordNumber + ",'" + node.getCategory() + "')");
        //result.append("t(" + formatCategory(node.getCategory()) + ", '" + node.getWord() + "', '" + Lemmatizer.lemmatize(node.getWord(), node.getPos()) + "', '" + node.getPos() + "', 'I-NP" + "', '" + node.getNER()  + "')");
        result.append("t(" + formatCategory(node.getCategory()) + ", '" + formatWord(node.getWord()) + "', '" + formatWord(Lemmatizer.lemmatizeWord(node.getWord())) +"', '"+node.getPos()+"', 'O', ' " + node.getNER() + " ')");
        //result.append("t(" + formatCategory(node.getCategory()) + ", '" + formatWord(node.getWord()) + "', '" + formatWord(Lemmatizer.lemmatize(node.getWord(), node.getPos())) +"', '', '', '')");
        wordNumber++;
      }
      private void printIndent(int currentIndent)
      {
        result.append(Strings.repeat(" ", currentIndent));
      }
    }

  }
  
  static class ExtendedCCGBankPrinter extends ParsePrinter {


    @Override
    void printFailure(StringBuilder result) {
    }

    class CCGBankParsePrinterVisitor extends ParsePrinterVisitor {
      // (<T S[wq] fa 0 2> (<L S[wq]/(S[dcl]\\NP) Who who WP O I-NP S[wq]/(S[dcl]\\NP)>) (<T S[dcl]\\NP rp 0 2> (<T S[dcl]\\NP ba 0 2> (<T S[dcl]\\NP fa 0 2> (<L (S[dcl]\\NP)/NP is be VBZ O I-VP (S[dcl]\\NP)/NP>) (<T NP ba 0 2> (<T NP[nb] fa 1 2> (<L NP[nb]/N the the DT O I-NP NP[nb]/N>) (<L N leader leader NN O I-NP N>)) (<T NP\\NP fa 0 2> (<L (NP\\NP)/NP of of IN O I-PP (NP\\NP)/NP>) (<T NP lex 0 1> (<L N Libya Libya NNP I-LOC I-NP N>))))) (<T (S\\NP)\\(S\\NP) fa 0 2> (<L ((S\\NP)\\(S\\NP))/NP in in IN O I-PP ((S[X]\\NP)\\(S[X]\\NP))/NP>) (<T NP lex 0 1> (<L N 2011 2011 CD I-DAT I-NP N>)))) (<L . ? ? . O O .>)))

      CCGBankParsePrinterVisitor(StringBuilder result) {
        super(result);
      }

      @Override
      public void visit(SyntaxTreeNodeBinary node)
      {
        result.append("(<T ");
        result.append(node.getCategory().toString());
        result.append(" " + PrologPrinter.getRuleName(node.getRuleType()) + " " + (node.headIsLeft ? "0" : "1") + " 2> ");
        node.leftChild.accept(this);
        node.rightChild.accept(this);

        result.append(") ");

      }

      @Override
      public void visit(SyntaxTreeNodeUnary node)
      {
        result.append("(<T ");
        result.append(node.getCategory().toString());
        result.append(" ");
        result.append("lex 0 1> ");
        node.child.accept(this);

        result.append(") ");

      }

      @Override
      public void visit(SyntaxTreeNodeLeaf node)
      {
        result.append("(<L ");
        result.append(node.getCategory().toString());
        result.append(" ");
        result.append(CCGBankPrinter.normalize(node.getWord()));
        result.append(" ");
        result.append(Lemmatizer_MIT.lemmatize(CCGBankPrinter.normalize(node.getWord()), node.getPos()));

        if (node.getPos() == null) {
          result.append(" NN ");
        } else {
          result.append(" " +node.getPos() + " ");
        }
        result.append(node.getNER());
        result.append(" O "); //ignoring chunking tags
        result.append(node.getCategory().toString());
        result.append(">) ");        
      }
    }

    @Override
    void printHeader(int id, StringBuilder result)
    {
      result.append("ID=" + id + "\n");
    }

    @Override
    void printFooter(StringBuilder result)
    {
    }

    @Override
    void printParse(SyntaxTreeNode parse, int sentenceNumber, StringBuilder result)
    {
      parse.accept(new CCGBankParsePrinterVisitor(result));
    }

    @Override
    void printFileHeader(StringBuilder result)
    {
    }
  
  }
}
