package asdl_base;
import java.io.*;

public  class Prims {

  private final static int set_neg_bit(int x) { 
    return (x | (0x40)); 
  }

  private final static boolean continue_bit_set(int x) {
    return ((x & (0x80)) != 0);
  }

  private final static boolean neg_bit_set(int x) {
    return ((x & (0x40)) != 0);
  }

  private static void die(String s) {
      System.err.println(s);
      System.exit(-1);
  }

  public static final void die() {
    die("Pkl error");
  } 

  public static final void write_tag(int x,OutputStream s) {
    write_int(x,s);
  }
  public static final void write_int(int x,OutputStream s) {
    boolean  is_neg  =  (x < 0) ;
    int v;
    try {
      if(is_neg) { x = -x; }
      
      while( x > 63) {
	v = ((x & 0x7F) | (0x80));
	s.write(v);
	x >>= 7;
      }
      
      if(is_neg) { x = set_neg_bit(x); }
      s.write(x);
    } catch (IOException e){
      die("Error writing int");
    }
  }

  public static final void write_String(String x,OutputStream s) {
    int sz = x.length();
    int i = 0;
    try {
      write_int(sz,s); 
      while(i < sz) {
	s.write((byte)x.charAt(i++));
      }
    } catch (IOException e){
      die("Error writing String");
    }
  }

  public static final void write_identifier(identifier x,OutputStream s) {
    write_String(x.toString(),s);
  }
  public static final int read_int(InputStream s) {
    int acc = 0;
    int shift = 0;
    int x;
    try {
  
      x = s.read();
      while(continue_bit_set(x)) {
	acc |= ((x & 0x7F)<<shift);
	shift+=7;
	x = s.read();
      }
      acc |= ((x & 0x3F) << shift);
      if(neg_bit_set(x)) {
	acc = -acc;
      }
    } catch (IOException e){
      die("Error reading int");
    }
    return acc;   
  }

  public static final int read_tag(InputStream s) {
    return read_int(s);
  }


 public static final String read_String(InputStream s) {

    
    int sz  = read_int(s);
    StringBuffer sb = new StringBuffer(sz);
    try {
      while(sz > 0) {
	sb.append((char)s.read());
	sz--;
      }
    } catch(IOException e) {
      die("Error reading int");
    }
    return sb.toString();
  }
 
  public static final identifier read_identifier(InputStream s) {
    return new identifier(read_String(s));
  }

  public static final void write_int_option(int_option i, OutputStream s) {
    if(i!=null) {
      write_int(1,s);
      write_int(i.x,s);
    } else {
      write_int(0,s);
    }
  }

  public static final int_option read_int_option(InputStream s) {
    if(read_int(s)!=0) {
      return new int_option(read_int(s));
    } else {
      return null;
    }
  }

  public static final void write_String_option(String_option i, 
					       OutputStream s) {
    if(i!=null) {
      write_int(1,s);
      write_String(i.x,s);
    } else {
      write_int(0,s);
    }
  }

  public static final String_option read_String_option(InputStream s) {
    if(read_int(s)!=0) {
      return new String_option(read_String(s));
    } else {
      return null;
    }
  }

  public static final void write_identifier_option(identifier_option i, 
					       OutputStream s) {
    if(i!=null) {
      write_int(1,s);
      write_identifier(i.x,s);
    } else {
      write_int(0,s);
    }
  }

  public static final identifier_option read_identifier_option(InputStream s) {
    if(read_int(s)!=0) {
      return new identifier_option(read_identifier(s));
    } else {
      return null;
    }
  }

  public static final int_list read_int_list(InputStream s)
  {
    int_list t;
    int t1;
    int_list t2;
    t1 = read_tag(s);
    if(t1 != 0)
      t = new int_list(read_int(s), null);
    else
      return null;
    t1 = t1 - 1;
    t2 = t;
    while(t1 != 0) {
	t2.tail = new int_list(read_int(s), null);
	t2 = t2.tail;
	t1 = t1 - 1;
      }
    return t;
  }

  
  public static final void write_int_list(int_list x,
						 java.io.OutputStream s)
  {
    int t1;
    int_list t2;
    t1 = 0;
    t2 = x;

    while(t2 != null) {
      t2 = t2.tail;
      t1 = t1 + 1;
    }
    write_tag(t1, s);
    t2 = x;
    while(t1 != 0) {
      write_int(t2.head, s);
      t2 = t2.tail;
      t1 = t1 - 1;
    }
  }
  public static final String_list read_String_list(InputStream s)
  {
    String_list t;
    int t1;
    String_list t2;
    t1 = read_tag(s);
    if(t1 != 0)
      t = new String_list(read_String(s), null);
    else
      return null;
    t1 = t1 - 1;
    t2 = t;
    while(t1 != 0) {
	t2.tail = new String_list(read_String(s), null);
	t2 = t2.tail;
	t1 = t1 - 1;
      }
    return t;
  }

  
  public static final void write_String_list(String_list x,
						 java.io.OutputStream s)
  {
    int t1;
    String_list t2;
    t1 = 0;
    t2 = x;

    while(t2 != null) {
      t2 = t2.tail;
      t1 = t1 + 1;
    }
    write_tag(t1, s);
    t2 = x;
    while(t1 != 0) {
      write_String(t2.head, s);
      t2 = t2.tail;
      t1 = t1 - 1;
    }
  }
  public static final identifier_list read_identifier_list(InputStream s)
  {
    identifier_list t;
    int t1;
    identifier_list t2;
    t1 = read_tag(s);
    if(t1 != 0)
      t = new identifier_list(read_identifier(s), null);
    else
      return null;
    t1 = t1 - 1;
    t2 = t;
    while(t1 != 0) {
	t2.tail = new identifier_list(read_identifier(s), null);
	t2 = t2.tail;
	t1 = t1 - 1;
      }
    return t;
  }

    public static final void write_identifier_list(identifier_list x,
						 java.io.OutputStream s)
  {
    int t1;
    identifier_list t2;
    t1 = 0;
    t2 = x;

    while(t2 != null) {
      t2 = t2.tail;
      t1 = t1 + 1;
    }
    write_tag(t1, s);
    t2 = x;
    while(t1 != 0) {
      write_identifier(t2.head, s);
      t2 = t2.tail;
      t1 = t1 - 1;
    }
  }

}

