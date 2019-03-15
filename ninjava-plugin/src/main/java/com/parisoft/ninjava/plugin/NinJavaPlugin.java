package com.parisoft.ninjava.plugin;

import com.sun.source.tree.ClassTree;
import com.sun.source.util.JavacTask;
import com.sun.source.util.Plugin;
import com.sun.source.util.TaskEvent;
import com.sun.source.util.TaskListener;
import com.sun.source.util.TreeScanner;
import com.sun.tools.javac.api.BasicJavacTask;
import com.sun.tools.javac.file.JavacFileManager;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.Log;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.tools.JavaFileManager;
import javax.tools.StandardLocation;

public class NinJavaPlugin implements Plugin {

    @Override
    public String getName() {
        return "NinJava";
    }

    @Override
    public void init(JavacTask javacTask, String... strings) {
        Context context = ((BasicJavacTask) javacTask).getContext();
        JavacFileManager fileManager = (JavacFileManager) context.get(JavaFileManager.class);
        Log log = Log.instance(context);
        log.printRawLines(Log.WriterKind.NOTICE, "Init " + getName());

        javacTask.addTaskListener(new TaskListener() {
            @Override
            public void started(TaskEvent taskEvent) {

            }

            @Override
            public void finished(TaskEvent taskEvent) {
                if (taskEvent.getKind() != TaskEvent.Kind.GENERATE) {
                    return;
                }

                taskEvent.getCompilationUnit().accept(new TreeScanner<Void, Void>() {
                    @Override
                    public Void visitClass(ClassTree classTree, Void aVoid) {
                        File out = fileManager.getLocation(StandardLocation.CLASS_OUTPUT).iterator().next();
                        String[] path = ((JCTree.JCFieldAccess) taskEvent.getCompilationUnit().getPackageName()).getIdentifier()
                                .toString()
                                .concat(".")
                                .concat(classTree.getSimpleName().toString())
                                .split("\\.");
                        path[path.length - 1] = path[path.length - 1].concat(".jasm");
                        Path asm = Paths.get(out.getAbsolutePath(), path);

                        try {
                            asm.toFile().getParentFile().mkdir();
                            Files.createFile(asm);
                        } catch (IOException e) {
                            log.error("Cannot create file " + asm + ": " + e.getMessage());
                        }

                        return super.visitClass(classTree, aVoid);
                    }
                }, null);
            }
        });
    }
}
