currentBuild.result = 'SUCCESS'
def caught_exception = null

final def EMAIL_RECIPIENTS = 'joey+git-annex@joeyh.name'

properties([
        buildDiscarder(logRotator(artifactNumToKeepStr: '1')),
        pipelineTriggers([[$class: 'hudson.triggers.SCMTrigger', scmpoll_spec: ''],])  // pollScm('') in 2.22+
])

try {

    node('windows') {

        dir('git-annex') {

            stage('Checkout') {
                checkout scm
            }

            stage('Build') {
                bat 'c:/msysgit/bin/sh standalone/windows/build.sh'
            }

            stage('Archive') {
                archiveArtifacts 'git-annex-installer.exe,dist/build-version'
            }

            stage('Upload') {
                withCredentials([usernamePassword(credentialsId: 'rsync-downloads-kitenet-net', passwordVariable: 'RSYNC_PASSWORD', usernameVariable: 'DUMMY')]) {
                    bat 'c:/cygwin/bin/rsync git-annex-installer.exe winautobuild@downloads.kitenet.net::winautobuild'
                    bat 'c:/cygwin/bin/rsync dist/build-version winautobuild@downloads.kitenet.net::winautobuild'
                }
            }

        }

    }

} catch (exception) {

    caught_exception = exception
    currentBuild.result = 'FAILURE'

} finally {

    node('master') {
        step([$class: 'Mailer', notifyEveryUnstableBuild: false, recipients: EMAIL_RECIPIENTS, sendToIndividuals: false])
    }

    if (caught_exception) {
        throw caught_exception
    }

}
