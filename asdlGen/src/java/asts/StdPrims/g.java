package asts.StdPrims;
import java.io.*;
import java.math.BigInteger;

public class g extends asts.StdPkl.g  {

  public static void write_int(int x, OutputStream s) { write_java_int(x,s); }
  public static int read_int(InputStream s) {  return read_java_int(s); }

  public static void write_String(String x,OutputStream s) {
    int sz = x.length();
    int i = 0;
    try {
      write_tag(sz,s); 
      while(i < sz) {
	s.write((byte)x.charAt(i++));
      }
    } catch (IOException e){
      die("Error writing String");
    }
  }

 public static String read_String(InputStream s) {

    
    int sz  = read_tag(s);
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
 

  public static identifier read_identifier(InputStream s) {
    return new identifier(read_String(s));
  }

  public static void write_identifier(identifier x,OutputStream s) {
    write_String(x.toString(),s);
  }





}

