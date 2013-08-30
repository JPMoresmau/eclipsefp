package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

/**
 * helper class for completion
 *
 * @author ??
 *
 */
public class HaskellCompletionContext  {

  private String source;
  private int fOffset;

	protected HaskellCompletionContext() {
		//placeholder constructor
	}

  public HaskellCompletionContext( final String source, final int offset ) {
    this.source = source;
    setOffset( offset );
  }


	protected void setOffset(final int fOffset) {
		this.fOffset = fOffset;
	}

	public int getOffset() {
		return fOffset;
	}



	public String getQualifier( ) {
	  return getQualifier( this.source, this.getOffset() );
	}

	public String getQualifier( final String source, final int offset )	{
		StringBuilder contents = readSourceTillOffset(source, offset);

		int index = offset;
		StringBuilder sb = new StringBuilder();
		String result = ""; //$NON-NLS-1$

		boolean finished = false;
		while( !finished && index > 0 ) {
			char ch = contents.charAt(--index);
			if( isIdentifierChar(ch) ) {
				sb.append( ch );
			} else if( ch == '\"' || ch == '\'' ) {
//				string or char literals are not taken into account
				finished = true;
			} else {
//				no more identifier part, so we use what we have collected
				result = sb.reverse().toString();
				finished = true;
			}
		}
		if( index == 0 ) {
//			the special case where we have collected sth. but have reached the
//			end of the document meanwhile
			result = sb.reverse().toString();
		}
		return result;
	}

	public String getPointedQualifier( ) {
    return getPointedQualifier( this.source, this.getOffset() );
  }

	public String getPointedQualifier( final String source, final int offset )  {
	  StringBuilder contents = readSourceTillOffset(source, offset);

    int index = offset;
    StringBuilder sb = new StringBuilder();
    String result = ""; //$NON-NLS-1$

    boolean finished = false;
    while( !finished && index > 0 ) {
      char ch = contents.charAt(--index);
      if( isIdentifierChar(ch) || ch == '.' ) {
        sb.append( ch );
      } else if( ch == '\"' || ch == '\'' ) {
//        string or char literals are not taken into account
        finished = true;
      } else {
//        no more identifier part, so we use what we have collected
        result = sb.reverse().toString();
        finished = true;
      }
    }
    if( index == 0 ) {
//      the special case where we have collected sth. but have reached the
//      end of the document meanwhile
      result = sb.reverse().toString();
    }
    return result;
  }

	private boolean isIdentifierChar(final char ch) {
		return Character.isLetterOrDigit(ch) || "_'".indexOf(ch) > -1; //$NON-NLS-1$
	}

	private StringBuilder readSourceTillOffset(final String source, final int offset)	{
		return new StringBuilder(source.substring(0, offset));
	}

}
