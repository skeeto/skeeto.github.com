function pd_plot (filename)
  fid = fopen (filename);
  
  cline = str2num (fgetl (fid));
  n = length (cline);
  fseek (fid, 0, SEEK_SET);
  
  c = 0;
  d = 0;
  while ~feof(fid)
    c = c + 1;
    printf ("Loading %d ...\n", c);
    fflush (stdout);
    
    cline = [];
    for i = 1:n
      cline = [cline; str2num(fgetl(fid))];
    end
    fgetl (fid);
    
    if (c == 1)
      d = max(cline(:)) + 1;
    end
    
    printf ("Saving %d ...\n", c);
    fflush (stdout);

    figure;
    imwrite (sprintf ("write-%.8d.png", c), cline+1, cmap(d));
  end
  
  close all;  
  fclose (fid);
end

function c = cmap (n)
  base = [.75 0 0; 0 0 1; 1 0 1; 0 0.5 0; 1 1 0; 0 1 1; 1 1 1; 0 0 0];
  c = repmat (base, ceil(n/length(base)), 1);
end
